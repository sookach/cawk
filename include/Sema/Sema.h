#pragma once

#include "AST/AST.h"
#include "Sema/SymbolResolver.h"
#include "Sema/SemaLValue.h"
#include "Sema/SemaType.h"

namespace cawk {
class Sema {
  SemaType TypeSema;
  SemaLValue LValueSema;

public:
  bool check(TranslationUnitDecl *T);
};

} // namespace cawk