#pragma once

#include "ast.h"
#include "token.h"
#include "token_type.h"

#include <fstream>
#include <iostream>

namespace cawk {
struct ast_code_gen final : public ast_visitor {
  std::ostream &os_{std::cout};

  ast_code_gen(std::ostream &os = std::cout) : os_{os} {}

  constexpr virtual void operator()(atom_expr &) override final;
  constexpr virtual void operator()(binary_expr &) override final;
  constexpr virtual void operator()(call_expr &) override final;
  constexpr virtual void operator()(cast_expr &) override final;
  constexpr virtual void operator()(field_expr &) override final;
  constexpr virtual void operator()(grouping_expr &) override final;
  constexpr virtual void operator()(index_expr &) override final;
  constexpr virtual void operator()(init_list_expr &) override final;
  constexpr virtual void operator()(postfix_expr &) override final;
  constexpr virtual void operator()(prefix_expr &) override final;

  constexpr virtual void operator()(templ &) override final;
  constexpr virtual void operator()(templ_type &) override final;

  constexpr virtual void operator()(block_stmt &) override final;
  constexpr virtual void operator()(break_stmt &) override final;
  constexpr virtual void operator()(exit_stmt &) override final;
  constexpr virtual void operator()(expr_stmt &) override final;
  constexpr virtual void operator()(for_stmt &) override final;
  constexpr virtual void operator()(if_stmt &) override final;
  constexpr virtual void operator()(print_stmt &) override final;
  constexpr virtual void operator()(range_stmt &) override final;
  constexpr virtual void operator()(return_stmt &) override final;
  constexpr virtual void operator()(switch_stmt &) override final;

  constexpr virtual void operator()(decl_stmt &) override final;
  constexpr virtual void operator()(fn_decl &) override final;
  constexpr virtual void operator()(rule_decl &) override final;
  constexpr virtual void operator()(var_decl &) override final;
};

constexpr void ast_code_gen::operator()(atom_expr &e) {
  switch (e.atom_.type_) {
  default:
    break;
  case token_type::char_constant:
    os_ << '\'';
    break;
  case token_type::string_literal:
    os_ << '"';
  }

  os_ << e.atom_.lexeme_;

  switch (e.atom_.type_) {
  default:
    break;
  case token_type::char_constant:
    os_ << '\'';
    break;
  case token_type::string_literal:
    os_ << '"';
  }
}

constexpr void ast_code_gen::operator()(binary_expr &e) {
  switch (e.op_.type_) {
  default:
    e.lhs_->operator()(this);
    os_ << e.op_.lexeme_;
    e.rhs_->operator()(this);
    break;
  case token_type::tilde:
    os_ << "match__(";
    e.lhs_->operator()(this);
    os_ << ',';
    e.rhs_->operator()(this);
    os_ << ')';
    break;
  case token_type::starstar:
    os_ << "pow_(";
    e.lhs_->operator()(this);
    os_ << ',';
    e.rhs_->operator()(this);
    os_ << ')';
    break;
  case token_type::slashslash:
    os_ << "pow_(";
    e.lhs_->operator()(this);
    os_ << ", 1.0 / (";
    e.rhs_->operator()(this);
    os_ << "))";
    break;
  case token_type::starstarequal:
    e.lhs_->operator()(this);
    os_ << "= pow_(";
    e.lhs_->operator()(this);
    os_ << ',';
    e.rhs_->operator()(this);
    os_ << ')';
    break;
  case token_type::slashslashequal:
    e.lhs_->operator()(this);
    os_ << "= pow_(";
    e.lhs_->operator()(this);
    os_ << ", 1.0 / (";
    e.rhs_->operator()(this);
    os_ << "))";
    break;
  }
}

constexpr void ast_code_gen::operator()(call_expr &e) {
  e.callee_->operator()(this);

  os_ << '(';

  if (!std::empty(e.args_)) {
    for (e.args_.front()->operator()(this);
         auto &&x : e.args_ | std::views::drop(1)) {
      os_ << ',';
      x->operator()(this);
    }
  }

  os_ << ')';
}

constexpr void ast_code_gen::operator()(cast_expr &e) {
  os_ << "cast__.operator()<" << e.type_.lexeme_ << ">(";
  e.e_->operator()(this);
  os_ << ')';
}

constexpr void ast_code_gen::operator()(field_expr &e) {
  os_ << "fields__[";
  e.e_->operator()(this);
  os_ << ']';
}

constexpr void ast_code_gen::operator()(grouping_expr &e) {
  os_ << '(';
  e.e_->operator()(this);
  os_ << ')';
}

constexpr void ast_code_gen::operator()(index_expr &e) {
  e.lhs_->operator()(this);
  os_ << '[';
  e.rhs_->operator()(this);
  os_ << ']';
}

constexpr void ast_code_gen::operator()(init_list_expr &e) {
  os_ << '{';
  if (!std::empty(e.init_list_)) {
    for (e.init_list_.front()->operator()(this);
         auto &&x : e.init_list_ | std::views::drop(1)) {
      os_ << ',';
      x->operator()(this);
    }
  }
  os_ << '}';
}

constexpr void ast_code_gen::operator()(postfix_expr &e) {
  e.lhs_->operator()(this);
  os_ << e.op_.lexeme_;
}

constexpr void ast_code_gen::operator()(prefix_expr &e) {
  os_ << e.op_.lexeme_;
  e.rhs_->operator()(this);
}

constexpr void ast_code_gen::operator()(templ &t) {
  if (os_ << '<'; !std::empty(t.types_)) {
    for (t.types_.front()->operator()(this);
         auto &&x : t.types_ | std::views::drop(1)) {
      os_ << ',';
      x->operator()(this);
    }
  }
  os_ << '>';
}

constexpr void ast_code_gen::operator()(templ_type &t) {
  if (os_ << t.iden_.lexeme_; t.t_ != nullptr)
    t.t_->operator()(this);
}

constexpr void ast_code_gen::operator()(break_stmt &s) { os_ << "break;"; }

constexpr void ast_code_gen::operator()(exit_stmt &s) {
  s.e_->operator()(this);
  os_ << ';';
}

constexpr void ast_code_gen::operator()(expr_stmt &s) {
  s.e_->operator()(this);
  os_ << ';';
}

constexpr void ast_code_gen::operator()(for_stmt &s) {
  os_ << "for(";
  if (s.init_ == nullptr)
    os_ << ';';
  else
    s.init_->operator()(this);
  if (s.cond_ != nullptr)
    s.cond_->operator()(this);
  os_ << ';';
  if (s.incr_ != nullptr)
    s.incr_->operator()(this);
  os_ << ')';
  if (s.body_ != nullptr)
    s.body_->operator()(this);
  else
    os_ << ';';
}

constexpr void ast_code_gen::operator()(if_stmt &s) {
  os_ << "if(";
  s.cond_->operator()(this);
  os_ << ')';
  s.then_->operator()(this);

  if (s.else_ != nullptr) {
    std::cout << "else ";
    s.else_->operator()(this);
  }
}

constexpr void ast_code_gen::operator()(print_stmt &s) {
  os_ << "std::cout";
  if (std::empty(s.args_))
    os_ << "<<fields__.front()";
  else {
    os_ << "<<";
    s.args_.front()->operator()(this);
    for (auto &&x : s.args_ | std::views::drop(1)) {
      os_ << "<< ' ' <<";
      x->operator()(this);
    }
  }
  os_ << "<< std::endl;";
}

constexpr void ast_code_gen::operator()(range_stmt &s) {
  os_ << "for (auto &&" << s.var_.lexeme_ << ':';
  s.range_->operator()(this);
  os_ << ')';
  if (s.body_ != nullptr)
    s.body_->operator()(this);
  else
    os_ << ';';
}

constexpr void ast_code_gen::operator()(return_stmt &s) {
  os_ << "return ";
  if (s.e_ != nullptr)
    s.e_->operator()(this);
  os_ << ';';
}

constexpr void ast_code_gen::operator()(switch_stmt &s) {
  os_ << "switch (";
  s.e_->operator()(this);
  os_ << ") {";
  for (auto &&x : s.cases_) {
    if (x.first.type_ != token_type::kw_default)
      os_ << "case ";
    if (x.first.type_ == token_type::string_literal)
      os_ << std::hash<std::string_view>{}(x.first.lexeme_) << "ul";
    else
      os_ << x.first.lexeme_;
    os_ << ":";
    x.second->operator()(this);
  }
  os_ << '}';
}

constexpr void ast_code_gen::operator()(block_stmt &d) {
  os_ << '{';
  for (auto &&x : d.body_)
    x->operator()(this);
  os_ << '}';
}

constexpr void ast_code_gen::operator()(decl_stmt &d) {
  d.s_->operator()(this);
}

constexpr void ast_code_gen::operator()(fn_decl &d) {
  os_ << (d.ret_ ? "auto " : "void ");
  os_ << d.iden_;

  if (std::empty(d.params_))
    os_ << "()";
  else {
    os_ << '(';
    os_ << "auto " << (d.params_.front().first ? "&&" : "")
        << (d.proto_ ? "" : d.params_.front().second);
    for (auto &&x : d.params_ | std::views::drop(1))
      os_ << ", auto " << (d.proto_ ? "" : (x.first ? "&&" : "") + x.second);
    os_ << ")";
  }

  if (d.proto_)
    os_ << ';';
  else {
    d.body_->operator()(this);
    os_ << "\n\n";
  }

  d.proto_ = false;
}

constexpr void ast_code_gen::operator()(rule_decl &d) {
  if (d.pattern_ != nullptr) {
    os_ << "if(";
    d.pattern_->operator()(this);
    os_ << ')';
  }

  if (d.action_ != nullptr)
    d.action_->operator()(this);
  else
    os_ << "std::cout << fields__.front() << std::endl;";
}

constexpr void ast_code_gen::operator()(var_decl &d) {
  if (d.is_static_)
    os_ << "static ";
  os_ << d.type_.lexeme_;

  if (d.temp_ != nullptr)
    d.temp_->operator()(this);

  os_ << ' ' << d.iden_.lexeme_;
  if (d.init_ != nullptr) {
    os_ << '=';
    d.init_->operator()(this);
  } else
    os_ << "{}";
  os_ << ';';
}

inline static struct {
  enum struct stage_label { all, support, fn_var, begin, mid, end };
  std::vector<std::unique_ptr<decl>> ast_{};

  constexpr void operator()(std::vector<std::unique_ptr<decl>> &&a) {
    ast_ = std::move(a);
  }

  template <stage_label stage = stage_label::all>
  constexpr void operator()(std::ostream &os) const {
    if constexpr (stage == stage_label::all) {
      this->operator()<stage_label::support>(os);
      this->operator()<stage_label::fn_var>(os);
      this->operator()<stage_label::begin>(os);
      this->operator()<stage_label::mid>(os);
      this->operator()<stage_label::end>(os);
    }

    if constexpr (stage == stage_label::support)
      os << std::ifstream{"include/cawk.h"}.rdbuf();

    if constexpr (stage == stage_label::fn_var) {
      os << "namespace cawk {";
      for (auto gen{std::make_unique<ast_code_gen>(os)}; auto &&x : ast_)
        if (x->kind_ == decl::kind::var || x->kind_ == decl::kind::fn)
          x->operator()(gen.get());

      for (auto gen{std::make_unique<ast_code_gen>(os)}; auto &&x : ast_)
        if (x->kind_ == decl::kind::fn)
          x->operator()(gen.get());
      os << "}";
    }

    if constexpr (stage == stage_label::begin) {
      os << "namespace cawk {";
      os << "auto init_begin__ {[]() noexcept -> bool {";
      os << "cawk::run_begin__ = [&]() noexcept -> void {";
      for (auto gen{std::make_unique<ast_code_gen>(os)}; auto &&x : ast_)
        if (x->kind_ == decl::kind::rule &&
            static_cast<cawk::rule_decl *>(x.get())->pos_ ==
                cawk::rule_decl::type::begin)
          x->operator()(gen.get());
      os << "};";
      os << "return true;";
      os << "}()};";
      os << "}";
    }

    if constexpr (stage == stage_label::mid) {
      os << "namespace cawk {";
      os << "auto init_mid__ {[]() noexcept -> bool {";
      os << "cawk::run_mid__ = [&]() noexcept -> void {";
      for (auto gen{std::make_unique<ast_code_gen>(os)}; auto &&x : ast_)
        if (x->kind_ == decl::kind::rule &&
            static_cast<cawk::rule_decl *>(x.get())->pos_ ==
                cawk::rule_decl::type::mid)
          x->operator()(gen.get());
      os << "};";
      os << "return true;";
      os << "}()};";
      os << "}";
    }

    if constexpr (stage == stage_label::end) {
      os << "namespace cawk {";
      os << "auto init_end__ {[]() noexcept -> bool {";
      os << "cawk::run_end__ = [&]() noexcept -> void {";
      for (auto gen{std::make_unique<ast_code_gen>(os)}; auto &&x : ast_)
        if (x->kind_ == decl::kind::rule &&
            static_cast<cawk::rule_decl *>(x.get())->pos_ ==
                cawk::rule_decl::type::end)
          x->operator()(gen.get());
      os << "};";
      os << "return true;";
      os << "}()};";
      os << "}";
    }
  }
} code_gen{};

} // namespace cawk