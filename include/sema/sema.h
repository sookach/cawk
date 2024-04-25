#include "ast.h"
#include "scope.h"
#include "token_type.h"

#include <iostream>

namespace cawk {
struct sema final : public ast_visitor {
  bool error_{};
  scope *scope_{};

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
  if ((error_ = e.atom_.type_ == token_type::identifier &&
                !scope_->contains(e.atom_.lexeme_)))
    std::cerr << "use of undeclared identifier " << e.atom_.lexeme_
              << std::endl;
}

constexpr void sema::operator()(binary_expr &) {}
constexpr void sema::operator()(call_expr &) {}
constexpr void sema::operator()(cast_expr &) {}
constexpr void sema::operator()(field_expr &) {}
constexpr void sema::operator()(grouping_expr &) {}
constexpr void sema::operator()(index_expr &) {}
constexpr void sema::operator()(init_list_expr &) {}
constexpr void sema::operator()(postfix_expr &) {}
constexpr void sema::operator()(prefix_expr &) {}

constexpr void sema::operator()(templ &) {}
constexpr void sema::operator()(templ_type &) {}

constexpr void sema::operator()(break_stmt &) {}
constexpr void sema::operator()(exit_stmt &) {}
constexpr void sema::operator()(expr_stmt &) {}
constexpr void sema::operator()(for_stmt &) {}
constexpr void sema::operator()(if_stmt &) {}
constexpr void sema::operator()(print_stmt &) {}
constexpr void sema::operator()(range_stmt &) {}
constexpr void sema::operator()(return_stmt &) {}
constexpr void sema::operator()(switch_stmt &) {}

constexpr void sema::operator()(block_stmt &) {
  // scope_ = new scope{scope_};

  // scope_ = scope_->get_parent();
}
constexpr void sema::operator()(decl_stmt &) {}
constexpr void sema::operator()(fn_decl &) {}
constexpr void sema::operator()(rule_decl &) {}
constexpr void sema::operator()(var_decl &) {}

} // namespace cawk