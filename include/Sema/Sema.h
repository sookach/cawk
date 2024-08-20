#pragma once

#include "AST/AST.h"
#include "Basic/Diagnostic.h"

namespace cawk {
class Sema {
  Diagnostic &Diags;

public:
  Sema(Diagnostic &Diags) : Diags(Diags) {}
  bool check(TranslationUnitDecl *T);
};

} // namespace cawk