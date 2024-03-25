//===- ast.h - AST node definitions ---------------------------------------===//
//
//  This file defines and impelements all of the AST interfaces.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "token.h"

#include <iostream>
#include <memory>
#include <ranges>

namespace cawk {
/// expr - The base struct from which all expressions derive.
struct expr {
  constexpr virtual void operator()(std::ostream &) const {};
};

/// binary_expr - A binary expression.
/// TODO: Maybe op_ should just be a token_type instead of a full token?
struct binary_expr final : public expr {
  const token op_{};
  const std::unique_ptr<expr> lhs_{}, rhs_{};

  constexpr binary_expr(token op, std::unique_ptr<expr> lhs,
                        std::unique_ptr<expr> rhs)
      : op_{op}, lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    switch (op_.type_) {
    default:
      lhs_->operator()(os);
      os << op_.lexeme_;
      rhs_->operator()(os);
      break;
    case token_type::tilde:
      os << "match__(";
      lhs_->operator()(os);
      os << ',';
      rhs_->operator()(os);
      os << ')';
      break;
    case token_type::starstar:
      os << "pow_(";
      lhs_->operator()(os);
      os << ',';
      rhs_->operator()(os);
      os << ')';
      break;
    case token_type::slashslash:
      os << "pow_(";
      lhs_->operator()(os);
      os << ", 1.0 / (";
      rhs_->operator()(os);
      os << "))";
      break;
    case token_type::starstarequal:
      lhs_->operator()(os);
      os << "= pow_(";
      lhs_->operator()(os);
      os << ',';
      rhs_->operator()(os);
      os << ')';
      break;
    case token_type::slashslashequal:
      lhs_->operator()(os);
      os << "= pow_(";
      lhs_->operator()(os);
      os << ", 1.0 / (";
      rhs_->operator()(os);
      os << "))";
      break;
    }
  }
};

/// grouping_expr - An expression enclosed in parentheses. Ensures that grouping
/// levels are properly maintained.
struct grouping_expr final : public expr {
  const std::unique_ptr<expr> e_{};

  constexpr grouping_expr(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << '(';
    e_->operator()(os);
    os << ')';
  }
};

/// prefix_expr - A standard prefix expression (!, ~, -, ++, --).
struct prefix_expr final : public expr {
  const token op_{};
  const std::unique_ptr<expr> rhs_{};

  constexpr prefix_expr(token op, std::unique_ptr<expr> rhs)
      : op_{op}, rhs_{std::move(rhs)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << op_.lexeme_;
    rhs_->operator()(os);
  }
};

/// postfix_expr - A standard postfix expression (++, --).
struct postfix_expr final : public expr {
  const token op_{};
  const std::unique_ptr<expr> lhs_{};

  constexpr postfix_expr(token op, std::unique_ptr<expr> lhs)
      : op_{op}, lhs_{std::move(lhs)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    lhs_->operator()(os);
    os << op_.lexeme_;
  }
};

/// index_expr - A indexing expression '[]'.
struct index_expr final : public expr {
  const std::unique_ptr<expr> lhs_{};
  const std::unique_ptr<expr> rhs_{};

  constexpr index_expr(std::unique_ptr<expr> lhs, std::unique_ptr<expr> rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    lhs_->operator()(os);
    os << '[';
    rhs_->operator()(os);
    os << ']';
  }
};

/// call_expr - A function call expression.
struct call_expr final : public expr {
  std::unique_ptr<expr> callee_{};
  std::vector<std::unique_ptr<expr>> args_{};

  constexpr call_expr(std::unique_ptr<expr> callee,
                      std::vector<std::unique_ptr<expr>> args = {})
      : callee_{std::move(callee)}, args_{std::move(args)} {}

  constexpr call_expr(std::unique_ptr<expr> callee, std::unique_ptr<expr> arg)
      : callee_{std::move(callee)} {
    args_.push_back(std::move(arg));
  }

  constexpr virtual void operator()(std::ostream &os) const override final {
    callee_->operator()(os);

    switch (os << '('; std::size(args_)) {
    default:
      for (args_.front()->operator()(os);
           auto &&x : args_ | std::views::drop(1)) {
        os << ',';
        x->operator()(os);
      }
      [[fallthrough]];
    case 0:
      break;
    case 1:
      args_.front()->operator()(os);
    }

    os << ')';
  }
};

/// field_expr - A field reference expression (i.e. $<expr>). Maybe can combined
/// this with prefix expression in the future.
struct field_expr final : public expr {
  std::unique_ptr<expr> e_{};

  constexpr field_expr(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "fields__[";
    e_->operator()(os);
    os << ']';
  }
};

/// cast_expr - A cast expression (i.e. ![<type>]). Just like field expressions,
/// maybe this too can be combined into prefix expressions.
struct cast_expr final : public expr {
  const token type_{};
  const std::unique_ptr<expr> e_{};

  constexpr cast_expr(token type, std::unique_ptr<expr> e)
      : type_{type}, e_{std::move(e)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "cast__.operator()<";
    os << type_.lexeme_;
    os << ">(";
    e_->operator()(os);
    os << ')';
  }
};

/// init_list_expr - An initializer list '{expr1, expr2, expr3}'.
struct init_list_expr final : public expr {
  std::vector<std::unique_ptr<expr>> init_list_{};

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << '{';
    if (!std::empty(init_list_)) {
      init_list_.front()->operator()(os);
      for (auto &&x : init_list_ | std::views::drop(1)) {
        os << ',';
        x->operator()(os);
      }
    }
    os << '}';
  }
};

/// atom_expr - Either a literal or symbol name.
struct atom_expr final : public expr {
  const token atom_{};

  constexpr atom_expr(token atom) : atom_{atom} {}

  constexpr atom_expr(token_type type, std::string lexeme)
      : atom_{.type_ = type, .lexeme_ = lexeme} {}

  virtual void operator()(std::ostream &os) const override final {
    switch (atom_.type_) {
    default:
      break;
    case token_type::char_constant:
      os << '\'';
      break;
    case token_type::string_literal:
      os << '"';
    }

    os << atom_.lexeme_;

    switch (atom_.type_) {
    default:
      break;
    case token_type::char_constant:
      os << '\'';
      break;
    case token_type::string_literal:
      os << '"';
    }
  }
};

/// templ - Template portion of a declaration.
struct templ final {
  struct templ_type final {
    token iden_{};
    std::unique_ptr<templ> t_{};

    templ_type(token iden = {}, std::unique_ptr<templ> t = nullptr)
        : iden_{iden}, t_{std::move(t)} {}

    constexpr void operator()(std::ostream &os) const {
      if (os << iden_.lexeme_; t_ != nullptr)
        t_->operator()(os);
    }
  };

  std::vector<std::unique_ptr<templ_type>> types_{};
  constexpr void operator()(std::ostream &os) const {
    os << '<';
    if (!std::empty(types_)) [[likely]] {
      types_.front()->operator()(os);
      for (auto &&x : types_ | std::views::drop(1)) {
        os << ',';
        x->operator()(os);
      }
    }
    os << '>';
  }
};

/// stmt - The base struct from which all statements and declarations derive.
struct stmt {
  constexpr virtual void operator()(std::ostream &) const {};
};

/// var_decl - A variable declaration.
struct var_decl final : public stmt {
  const bool is_static_{};
  const token type_{};
  const std::unique_ptr<templ> temp_{};
  const token iden_{};
  const std::unique_ptr<expr> init_{};

  constexpr var_decl(token type, token iden,
                     std::unique_ptr<expr> init = nullptr)
      : type_{type}, iden_{iden}, init_{std::move(init)} {}

  constexpr var_decl(token type, std::unique_ptr<templ> temp, token iden,
                     std::unique_ptr<expr> init = nullptr)
      : type_{type}, temp_{std::move(temp)}, iden_{iden},
        init_{std::move(init)} {}

  constexpr var_decl(bool is_static, token type, std::unique_ptr<templ> temp,
                     token iden, std::unique_ptr<expr> init = nullptr)
      : is_static_{is_static}, type_{type}, temp_{std::move(temp)}, iden_{iden},
        init_{std::move(init)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    if (is_static_)
      os << "static ";
    os << type_.lexeme_;

    if (temp_ != nullptr)
      temp_->operator()(os);

    os << ' ' << iden_.lexeme_;
    if (init_ != nullptr) {
      os << '=';
      init_->operator()(os);
    } else
      os << "{}";
    os << ';';
  }
};

/// fn_decl - A function declaration (really a definition since CAWK doesn't
/// have function declarations).
struct fn_decl final : public stmt {
  const std::string iden_{};
  const std::unique_ptr<stmt> body_{};
  const std::vector<std::pair<bool, std::string>> params_{};
  const bool ret_{};
  mutable bool proto_{true};

  constexpr fn_decl(std::string iden, std::unique_ptr<stmt> body,
                    std::vector<std::pair<bool, std::string>> params = {},
                    bool ret = false)
      : iden_{iden}, body_{std::move(body)}, params_{params}, ret_{ret} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << (ret_ ? "auto " : "void ");
    os << iden_;

    if (std::empty(params_))
      os << "()";
    else {
      os << '(';
      os << "auto " << (params_.front().first ? "&&" : "")
         << (proto_ ? "" : params_.front().second);
      for (auto &&x : params_ | std::views::drop(1))
        os << ", auto " << (proto_ ? "" : (x.first ? "&&" : "") + x.second);
      os << ")";
    }

    if (proto_)
      os << ';';
    else {
      body_->operator()(os);
      os << "\n\n";
    }

    proto_ = false;
  }
};

/// block_stmt - A block statement.
struct block_stmt final : public stmt {
  const std::vector<std::unique_ptr<stmt>> body_{};

  constexpr block_stmt(std::vector<std::unique_ptr<stmt>> body)
      : body_{std::move(body)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << '{';
    for (auto &&x : body_)
      x->operator()(os);
    os << '}';
  }
};

/// expr_stmt - An expression statement (i.e. an expression which a semi at the
/// end).
struct expr_stmt final : public stmt {
  const std::unique_ptr<expr> e_{};

  constexpr expr_stmt(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    if (e_ != nullptr)
      e_->operator()(os);
    os << ';';
  }
};

/// if_stmt - An if statement.
struct if_stmt final : public stmt {
  const std::unique_ptr<expr> cond_{};
  const std::unique_ptr<stmt> then_{};
  const std::unique_ptr<stmt> else_{};

  constexpr if_stmt(std::unique_ptr<expr> c, std::unique_ptr<stmt> t,
                    std::unique_ptr<stmt> e = nullptr)
      : cond_{std::move(c)}, then_{std::move(t)}, else_{std::move(e)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "if(";
    cond_->operator()(os);
    os << ')';
    then_->operator()(os);

    if (else_ != nullptr) {
      std::cout << "else";
      else_->operator()(os);
    }
  }
};

/// for_stmt - A traditional C-style for loop.
struct for_stmt final : public stmt {
  const std::unique_ptr<stmt> init_{};
  const std::unique_ptr<expr> cond_{};
  const std::unique_ptr<expr> incr_{};
  const std::unique_ptr<stmt> body_{};

  constexpr for_stmt(std::unique_ptr<stmt> init, std::unique_ptr<expr> cond,
                     std::unique_ptr<expr> incr, std::unique_ptr<stmt> body)
      : init_{std::move(init)}, cond_{std::move(cond)}, incr_{std::move(incr)},
        body_{std::move(body)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "for(";
    if (init_ == nullptr)
      os << ';';
    else
      init_->operator()(os);
    if (cond_ != nullptr)
      cond_->operator()(os);
    os << ';';
    if (incr_ != nullptr)
      incr_->operator()(os);
    os << ')';
    if (body_ != nullptr)
      body_->operator()(os);
    else
      os << ';';
  }
};

/// print_stmt - 'print' followed by an expression statement.
struct print_stmt final : public stmt {
  const std::vector<std::unique_ptr<expr>> args_{};

  constexpr print_stmt(std::vector<std::unique_ptr<expr>> args = {})
      : args_{std::move(args)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "std::cout";
    if (std::empty(args_))
      os << "<<fields__.front()";
    else {
      os << "<<";
      args_.front()->operator()(os);
      for (auto &&x : args_ | std::views::drop(1)) {
        os << "<< ' ' <<";
        x->operator()(os);
      }
    }
    os << "<< std::endl;";
  }
};

/// return_stmt - A return stmt.
struct return_stmt final : public stmt {
  const std::unique_ptr<expr> value_{};

  constexpr return_stmt(std::unique_ptr<expr> value = nullptr)
      : value_{std::move(value)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "return";

    if (value_ != nullptr) {
      os << ' ';
      value_->operator()(os);
    }

    os << ';';
  }
};

/// range_stmt - A range for loop 'for (x in range)'.
struct range_stmt final : public stmt {
  const token var_{};
  const std::unique_ptr<expr> range_{};
  const std::unique_ptr<stmt> body_{};

  constexpr range_stmt(token var, std::unique_ptr<expr> range,
                       std::unique_ptr<stmt> body)
      : var_{var}, range_{std::move(range)}, body_{std::move(body)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "for(auto &&" << var_.lexeme_ << ':';
    range_->operator()(os);
    os << ')';
    if (body_ != nullptr)
      body_->operator()(os);
    else
      os << ';';
  }
};

/// break_stmt - A break statement.
struct break_stmt final : public stmt {
  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "break;";
  }
};

/// switch_stmt - A switch statement.
struct switch_stmt final : public stmt {
  std::unique_ptr<expr> e_{};
  std::vector<std::pair<token, std::unique_ptr<block_stmt>>> cases_{};

  constexpr switch_stmt(std::unique_ptr<expr> e, decltype(cases_) cases)
      : e_{std::move(e)}, cases_{std::move(cases)} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    os << "switch (";
    e_->operator()(os);
    os << ") {";
    for (auto &&x : cases_) {
      if (x.first.type_ != token_type::kw_default)
        os << "case ";
      if (x.first.type_ == token_type::string_literal)
        os << std::hash<std::string_view>{}(x.first.lexeme_) << "ul";
      else
        os << x.first.lexeme_;
      os << ":";
      x.second->operator()(os);
    }
    os << '}';
  }
};

/// pattern_action_decl - A rule definition.
struct pattern_action_decl final : public stmt {
  const std::unique_ptr<expr> pattern_{};
  const std::unique_ptr<stmt> action_{};
  const enum struct type { begin, mid, end } pos_{};

  constexpr pattern_action_decl(std::unique_ptr<expr> pattern,
                                std::unique_ptr<stmt> action, type pos)
      : pattern_{std::move(pattern)}, action_{std::move(action)}, pos_{pos} {}

  constexpr virtual void operator()(std::ostream &os) const override final {
    if (pattern_ != nullptr) {
      os << "if(";
      pattern_->operator()(os);
      os << ')';
    }

    if (action_ != nullptr)
      action_->operator()(os);
    else
      os << "std::cout << fields__.front() << std::endl;";
  }
};

} // namespace cawk