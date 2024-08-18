#pragma once

#include "AST/AST.h"
#include "Sema/SemaControlFlow.h"
#include "Sema/SemaLValue.h"
#include "Sema/SemaType.h"
#include "Sema/SymbolResolver.h"

namespace cawk {
class Sema {
  SemaType TypeSema;
  SemaLValue LValueSema;
  SemaControlFlow ControlFlowSema;
  SymbolResolver Resolver;

public:
  bool check(TranslationUnitDecl *T);
};

} // namespace cawk