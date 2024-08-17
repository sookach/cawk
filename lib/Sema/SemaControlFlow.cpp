#include "Sema/SemaControlFlow.h"

#include "Support/Support.h"

using namespace cawk;

template <bool FirstVisit> bool SemaControlFlow::visit(FunctionDecl *F) {
  if constexpr (FirstVisit)
    InFunction = true;
  else
    InFunction = false;
  return true;
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

template <bool> bool SemaControlFlow::visit(ExitStmt *E) { return true; }

template <bool FirstVisit> bool SemaControlFlow::visit(ForStmt *F) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

template <bool FirstVisit> bool SemaControlFlow::visit(ForRangeStmt *F) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

template <bool> bool SemaControlFlow::visit(IfStmt *I) { return true; }

template <bool> bool SemaControlFlow::visit(NextStmt *N) { return true; }

template <bool> bool SemaControlFlow::visit(NextfileStmt *N) { return true; }

template <bool> bool SemaControlFlow::visit(PrintStmt *P) { return true; }

template <bool> bool SemaControlFlow::visit(ReturnStmt *R) {
  return InFunction;
}

template <bool> bool SemaControlFlow::visit(ValueStmt *V) { return true; }

template <bool FirstVisit> bool SemaControlFlow::visit(WhileStmt *W) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

template <bool> bool SemaControlFlow::visit(ArraySubscriptExpr *A) {
  return true;
}

template <bool> bool SemaControlFlow::visit(BinaryOperator *B) { return true; }

template <bool> bool SemaControlFlow::visit(CallExpr *C) { return true; }

template <bool> bool SemaControlFlow::visit(DeclRefExpr *D) { return true; }

template <bool> bool SemaControlFlow::visit(FloatingLiteral *F) { return true; }

template <bool> bool SemaControlFlow::visit(RegexLiteral *R) { return true; }

template <bool> bool SemaControlFlow::visit(StringLiteral *S) { return true; }

template <bool> bool SemaControlFlow::visit(UnaryOperator *U) { return true; }

bool SemaControlFlow::check(TranslationUnitDecl *T) { return traverse(T); }