#include "Sema/Sema.h"

using namespace cawk;

bool Sema::check(TranslationUnitDecl *T) {
  TypeSema.check(T);
  LValueSema.check(T);
  ControlFlowSema.check(T);
  return true;
}