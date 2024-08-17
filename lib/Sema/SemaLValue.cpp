#include "Sema/SemaLValue.h"

#include "Support/Support.h"

using namespace cawk;

bool SemaLValue::visit(FunctionDecl *F) { return true; }

bool SemaLValue::visit(ParamVarDecl *P) { return true; }

bool SemaLValue::visit(RuleDecl *R) { return true; }

bool SemaLValue::visit(TranslationUnitDecl *T) { return true; }

bool SemaLValue::visit(VarDecl *V) { return true; }

bool SemaLValue::visit(BreakStmt *B) { return true; }

bool SemaLValue::visit(CompoundStmt *C) { return true; }

bool SemaLValue::visit(ContinueStmt *C) { return true; }

bool SemaLValue::visit(DeleteStmt *D) { return D->getArgument()->isLValue(); }

bool SemaLValue::visit(DoStmt *D) { return true; }

bool SemaLValue::visit(ExitStmt *E) { return true; }

bool SemaLValue::visit(ForStmt *F) { return true; }

bool SemaLValue::visit(ForRangeStmt *F) {
  return F->getLoopVar()->isLValue() && F->getRange()->isLValue();
}

bool SemaLValue::visit(IfStmt *I) { return true; }

bool SemaLValue::visit(NextStmt *N) { return true; }

bool SemaLValue::visit(NextfileStmt *N) { return true; }

bool SemaLValue::visit(PrintStmt *P) { return true; }

bool SemaLValue::visit(ReturnStmt *R) { return true; }

bool SemaLValue::visit(ValueStmt *V) { return true; }

bool SemaLValue::visit(WhileStmt *W) { return true; }

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

bool SemaLValue::visit(CallExpr *C) { return true; }

bool SemaLValue::visit(DeclRefExpr *D) {
  D->markAsLValue();
  return true;
}

bool SemaLValue::visit(FloatingLiteral *F) { return true; }

bool SemaLValue::visit(RegexLiteral *R) { return true; }

bool SemaLValue::visit(StringLiteral *S) { return true; }

bool SemaLValue::visit(UnaryOperator *U) { return true; }

bool SemaLValue::check(TranslationUnitDecl *T) { return traverse(T); }