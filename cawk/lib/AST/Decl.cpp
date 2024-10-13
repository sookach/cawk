#include "cawk/AST/Decl.h"
#include "cawk/AST/Expr.h"

using namespace cawk;

std::string_view VarDecl::getName() {
  return D->getIdentifier().getIdentifier();
}