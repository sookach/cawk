#include "ast.h"
#include "token_type.h"

#include <iostream>

namespace cawk {
struct sema final : public ast_visitor {
  bool error_{};

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

  constexpr virtual void operator()(break_stmt &) override final;
  constexpr virtual void operator()(exit_stmt &) override final;
  constexpr virtual void operator()(expr_stmt &) override final;
  constexpr virtual void operator()(for_stmt &) override final;
  constexpr virtual void operator()(if_stmt &) override final;
  constexpr virtual void operator()(print_stmt &) override final;
  constexpr virtual void operator()(range_stmt &) override final;
  constexpr virtual void operator()(return_stmt &) override final;
  constexpr virtual void operator()(switch_stmt &) override final;

  constexpr virtual void operator()(block_stmt &) override final;
  constexpr virtual void operator()(decl_stmt &) override final;
  constexpr virtual void operator()(fn_decl &) override final;
  constexpr virtual void operator()(rule_decl &) override final;
  constexpr virtual void operator()(var_decl &) override final;
};

constexpr void sema::operator()(atom_expr &e) {
  switch (e.atom_.type_) {
  default:
    std::cerr << "invalid expression type: " << e.atom_.type_ << '\n';
    error_ = true;
    break;
  case token_type::numeric_constant:
    e.type_ = expr::type::numeric_t;
    break;
  case token_type::string_literal:
    e.type_ = expr::type::string_t;
    break;
  case token_type::char_constant:
    e.type_ = expr::type::char_t;
    break;
  case token_type::kw_true:
    [[fallthrough]];
  case token_type::kw_false:
    e.type_ = expr::type::bool_t;
  }
}

constexpr void sema::operator()(binary_expr &e) {
  switch (e.lhs_->type_) {
  default:
    break;
  }
}

} // namespace cawk