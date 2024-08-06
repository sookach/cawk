#include "Sema/SemaControlFlow.h"

#include "Support/Support.h"

using namespace cawk;

template <bool FirstVisit> bool SemaControlFlow::visit(FunctionDecl *F) {
  if constexpr (FirstVisit)
    InFunction = true;
  else
    InFunction = false;
}

template <bool> bool SemaControlFlow::visit(ParamVarDecl *P) { return true; }

template <bool> bool SemaControlFlow::visit(RuleDecl *R) { return true; }

template <bool> bool SemaControlFlow::visit(TranslationUnitDecl *T) {
  return true;
}

template <bool> bool SemaControlFlow::visit(VarDecl *V) { return true; }

template <bool> bool SemaControlFlow::visit(BreakStmt *B) { return isInLoop(); }

template <bool> bool SemaControlFlow::visit(CompoundStmt *C) { return true; }

template <bool> bool SemaControlFlow::visit(ContinueStmt *C) {
  return isInLoop();
}

template <bool> bool SemaControlFlow::visit(DeleteStmt *D) { return true; }

template <bool FirstVisit> bool SemaControlFlow::visit(DoStmt *D) {
  enterLoop();
  return true;
}

bool SemaControlFlow::visit(ExitStmt *E) { return true; }

template <bool FirstVisit> bool SemaControlFlow::visit(ForStmt *F) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

bool SemaControlFlow::visit(ForRangeStmt *F) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

bool SemaControlFlow::visit(IfStmt *I) { return true; }

bool SemaControlFlow::visit(NextStmt *N) { return true; }

bool SemaControlFlow::visit(NextfileStmt *N) { return true; }

bool SemaControlFlow::visit(PrintStmt *P) { return true; }

bool SemaControlFlow::visit(ReturnStmt *R) { return InFunction; }

bool SemaControlFlow::visit(ValueStmt *V) { return true; }

bool SemaControlFlow::visit(WhileStmt *W) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

bool SemaControlFlow::visit(ArraySubscriptExpr *A) { return true; }

bool SemaControlFlow::visit(BinaryOperator *B) { return true; }

bool SemaControlFlow::visit(CallExpr *C) { return true; }

bool SemaControlFlow::visit(DeclRefExpr *D) { return true; }

bool SemaControlFlow::visit(FloatingLiteral *F) { return true; }

bool SemaControlFlow::visit(RegexLiteral *R) { return true; }

bool SemaControlFlow::visit(StringLiteral *S) { return true; }

bool SemaControlFlow::visit(UnaryOperator *U) { return true; }

bool SemaControlFlow::check(TranslationUnitDecl *T) { return true; }