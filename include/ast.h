//===- ast.h - AST node definitions ---------------------------------------===//
//
//  This file defines all of the AST interfaces.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "token.h"

#include <iostream>
#include <memory>
#include <ranges>
#include <vector>

namespace cawk {

// Expression AST node forward declarations.

struct expr;
struct atom_expr;
struct binary_expr;
struct call_expr;
struct cast_expr;
struct field_expr;
struct grouping_expr;
struct index_expr;
struct init_list_expr;
struct postfix_expr;
struct prefix_expr;

// Template AST node forward declaration.

struct templ;
struct templ_type;

// Statement AST node forward declarations.

struct stmt;
struct break_stmt;
struct exit_stmt;
struct expr_stmt;
struct for_stmt;
struct if_stmt;
struct print_stmt;
struct range_stmt;
struct return_stmt;
struct switch_stmt;

// Declaration AST node forward declarations.

struct decl;
struct block_stmt;
struct decl_stmt;
struct fn_decl;
struct rule_decl;
struct var_decl;

struct ast_visitor {
  constexpr virtual void operator()(atom_expr &) = 0;
  constexpr virtual void operator()(binary_expr &) = 0;
  constexpr virtual void operator()(call_expr &) = 0;
  constexpr virtual void operator()(cast_expr &) = 0;
  constexpr virtual void operator()(field_expr &) = 0;
  constexpr virtual void operator()(grouping_expr &) = 0;
  constexpr virtual void operator()(index_expr &) = 0;
  constexpr virtual void operator()(init_list_expr &) = 0;
  constexpr virtual void operator()(postfix_expr &) = 0;
  constexpr virtual void operator()(prefix_expr &) = 0;

  constexpr virtual void operator()(templ &) = 0;
  constexpr virtual void operator()(templ_type &) = 0;

  constexpr virtual void operator()(block_stmt &) = 0;
  constexpr virtual void operator()(break_stmt &) = 0;
  constexpr virtual void operator()(exit_stmt &) = 0;
  constexpr virtual void operator()(expr_stmt &) = 0;
  constexpr virtual void operator()(for_stmt &) = 0;
  constexpr virtual void operator()(if_stmt &) = 0;
  constexpr virtual void operator()(print_stmt &) = 0;
  constexpr virtual void operator()(range_stmt &) = 0;
  constexpr virtual void operator()(return_stmt &) = 0;
  constexpr virtual void operator()(switch_stmt &) = 0;

  constexpr virtual void operator()(decl_stmt &) = 0;
  constexpr virtual void operator()(fn_decl &) = 0;
  constexpr virtual void operator()(rule_decl &) = 0;
  constexpr virtual void operator()(var_decl &) = 0;
};

struct ast {
  virtual ~ast() {}
  virtual void operator()(ast_visitor *v) = 0;
};

/// expr - The base struct from which all expressions derive.
struct expr : public ast {
  enum struct type { unknown_t, numeric_t, string_t, char_t, bool_t } type_{};
  virtual ~expr() {}
};

/// stmt - The base struct from which all statements derive.
struct stmt : public ast {
  virtual ~stmt() {}
};

/// decl - The base struct from which all declarations derive.
struct decl : public ast {
  const token iden_{};
  virtual ~decl() {}
};

/// atom_expr - Either a literal or symbol name.
struct atom_expr final : public expr {
  const token atom_{};

  constexpr atom_expr(token atom) : atom_{atom} {}

  constexpr atom_expr(token_type type, std::string lexeme)
      : atom_{.type_ = type, .lexeme_ = lexeme} {}

  virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// binary_expr - A binary expression.
/// TODO: Maybe op_ should just be a token_type instead of a full token?
struct binary_expr final : public expr {
  const token op_{};
  const std::unique_ptr<expr> lhs_{}, rhs_{};

  constexpr binary_expr(token op, std::unique_ptr<expr> lhs,
                        std::unique_ptr<expr> rhs)
      : op_{op}, lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  virtual void operator()(ast_visitor *v) override { v->operator()(*this); }
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

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// cast_expr - A cast expression (i.e. ![<type>]). Just like field expressions,
/// maybe this too can be combined into prefix expressions.
struct cast_expr final : public expr {
  const token type_{};
  const std::unique_ptr<expr> e_{};

  constexpr cast_expr(token type, std::unique_ptr<expr> e)
      : type_{type}, e_{std::move(e)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// field_expr - A field reference expression (i.e. $<expr>). Maybe can combined
/// this with prefix expression in the future.
struct field_expr final : public expr {
  std::unique_ptr<expr> e_{};

  constexpr field_expr(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// grouping_expr - An expression enclosed in parentheses. Ensures that grouping
/// levels are properly maintained.
struct grouping_expr final : public expr {
  const std::unique_ptr<expr> e_{};

  constexpr grouping_expr(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// index_expr - A indexing expression '[]'.
struct index_expr final : public expr {
  const std::unique_ptr<expr> lhs_{};
  const std::unique_ptr<expr> rhs_{};

  constexpr index_expr(std::unique_ptr<expr> lhs, std::unique_ptr<expr> rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// init_list_expr - An initializer list '{expr1, expr2, expr3}'.
struct init_list_expr final : public expr {
  std::vector<std::unique_ptr<expr>> init_list_{};

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// postfix_expr - A standard postfix expression (++, --).
struct postfix_expr final : public expr {
  const token op_{};
  const std::unique_ptr<expr> lhs_{};

  constexpr postfix_expr(token op, std::unique_ptr<expr> lhs)
      : op_{op}, lhs_{std::move(lhs)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// prefix_expr - A standard prefix expression (!, ~, -, ++, --).
struct prefix_expr final : public expr {
  const token op_{};
  const std::unique_ptr<expr> rhs_{};

  constexpr prefix_expr(token op, std::unique_ptr<expr> rhs)
      : op_{op}, rhs_{std::move(rhs)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// templ - Template portion of a declaration.
struct templ final : public ast {
  std::vector<std::unique_ptr<templ_type>> types_{};

  constexpr templ() = default;

  constexpr void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

struct templ_type final : public ast {
  token iden_{};
  std::unique_ptr<templ> t_{};

  constexpr templ_type(token iden = {}, std::unique_ptr<templ> t = nullptr)
      : iden_{iden}, t_{std::move(t)} {}

  constexpr void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// block_stmt - A block statement.
struct block_stmt final : public stmt {
  const std::vector<std::unique_ptr<decl>> body_{};

  constexpr block_stmt(std::vector<std::unique_ptr<decl>> body)
      : body_{std::move(body)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// break_stmt - A break statement.
struct break_stmt final : public stmt {
  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// expr_stmt - An exit statement.
struct exit_stmt final : public stmt {
  const std::unique_ptr<expr> e_{};

  constexpr exit_stmt(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// expr_stmt - An expression statement (i.e. an expression which a semi at the
/// end).
struct expr_stmt final : public stmt {
  const std::unique_ptr<expr> e_{};

  constexpr expr_stmt(std::unique_ptr<expr> e) : e_{std::move(e)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// decl_stmt - An adapter struct for mixing declarations with statements.
struct decl_stmt final : public decl {
  std::unique_ptr<stmt> s_{};

  constexpr decl_stmt(std::unique_ptr<stmt> s) : s_{std::move(s)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// for_stmt - A traditional C-style for loop.
struct for_stmt final : public stmt {
  const std::unique_ptr<decl> init_{};
  const std::unique_ptr<expr> cond_{};
  const std::unique_ptr<expr> incr_{};
  const std::unique_ptr<stmt> body_{};

  constexpr for_stmt(std::unique_ptr<decl> init, std::unique_ptr<expr> cond,
                     std::unique_ptr<expr> incr, std::unique_ptr<stmt> body)
      : init_{std::move(init)}, cond_{std::move(cond)}, incr_{std::move(incr)},
        body_{std::move(body)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
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

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// print_stmt - 'print' followed by an expression statement.
struct print_stmt final : public stmt {
  const std::vector<std::unique_ptr<expr>> args_{};

  constexpr print_stmt(std::vector<std::unique_ptr<expr>> args = {})
      : args_{std::move(args)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// return_stmt - A return stmt.
struct return_stmt final : public stmt {
  const std::unique_ptr<expr> e_{};

  constexpr return_stmt(std::unique_ptr<expr> e = nullptr) : e_{std::move(e)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
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

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// switch_stmt - A switch statement.
struct switch_stmt final : public stmt {
  std::unique_ptr<expr> e_{};
  std::vector<std::pair<token, std::unique_ptr<block_stmt>>> cases_{};

  constexpr switch_stmt(std::unique_ptr<expr> e, decltype(cases_) cases)
      : e_{std::move(e)}, cases_{std::move(cases)} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// fn_decl - A function declaration (really a definition since CAWK doesn't
/// have function declarations).
struct fn_decl final : public decl {
  const std::string iden_{};
  const std::unique_ptr<block_stmt> body_{};
  const std::vector<std::pair<bool, std::string>> params_{};
  const bool ret_{};
  mutable bool proto_{true};

  constexpr fn_decl(std::string iden, std::unique_ptr<block_stmt> body,
                    std::vector<std::pair<bool, std::string>> params = {},
                    bool ret = false)
      : iden_{iden}, body_{std::move(body)}, params_{params}, ret_{ret} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// rule_decl - A rule definition.
struct rule_decl final : public decl {
  const std::unique_ptr<expr> pattern_{};
  const std::unique_ptr<block_stmt> action_{};
  const enum struct type { begin, mid, end } pos_{};

  constexpr rule_decl(std::unique_ptr<expr> pattern,
                      std::unique_ptr<block_stmt> action, type pos)
      : pattern_{std::move(pattern)}, action_{std::move(action)}, pos_{pos} {}

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

/// var_decl - A variable declaration. For now the only declarator is static.
/// Variables without initializer are default initialized.
struct var_decl final : public decl {
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

  constexpr virtual void operator()(ast_visitor *v) override final {
    v->operator()(*this);
  }
};

} // namespace cawk