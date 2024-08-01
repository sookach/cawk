#pragma once

#include "AST/AST.h"
#include "Sema/SemaDecl.h"
#include "Sema/SemaLValue.h"
#include "Sema/SemaType.h"

namespace cawk {
class Sema {
  SemaDecl DeclSema;
  SemaType TypeSema;
  SemaLValue LValueSema;

public:
  bool check(TranslationUnitDecl *T);
};

} // namespace cawk