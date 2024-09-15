#include "AST/Decl.h"
#include "AST/Expr.h"

using namespace cawk;

std::string_view VarDecl::getName() {
  return D->getIdentifier().getIdentifier();
}