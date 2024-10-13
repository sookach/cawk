#include "cawk/Sema/SemaLValue.h"
#include "cawk/Support/Support.h"

using namespace cawk;

bool SemaLValue::visit(DeleteStmt *D) { return D->getArgument()->isLValue(); }

bool SemaLValue::visit(ForRangeStmt *F) {
  return F->getLoopVar()->isLValue() && F->getRange()->isLValue();
}

bool SemaLValue::visit(ArraySubscriptExpr *A) {
  A->markAsLValue();
  return true;
}

bool SemaLValue::visit(BinaryOperator *B) {
  switch (B->getOpcode().getKind()) {
  default:
    break;
  case tok::equal:
  case tok::plusequal:
  case tok::minusequal:
  case tok::starequal:
  case tok::slashequal:
  case tok::percentequal:
  case tok::caretequal:
  case tok::starstarequal:
    if (!B->getLHS()->isLValue())
      return false;
    B->markAsLValue();
  }
  return true;
}

bool SemaLValue::visit(DeclRefExpr *D) {
  D->markAsLValue();
  return true;
}

bool SemaLValue::check(TranslationUnitDecl *T) { return traverse(T); }