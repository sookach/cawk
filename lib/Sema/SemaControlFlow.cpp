#include "Sema/SemaControlFlow.h"
#include "Basic/Diagnostic.h"
#include "Support/Support.h"

using namespace cawk;

template <bool FirstVisit> bool SemaControlFlow::check(FunctionDecl *F) {
  InFunction = FirstVisit;
  return true;
}

template bool SemaControlFlow::check<true>(FunctionDecl *);
template bool SemaControlFlow::check<false>(FunctionDecl *);

bool SemaControlFlow::check(BreakStmt *B) {
  if (isInLoop())
    return true;
  return false;
}

bool SemaControlFlow::check(ContinueStmt *C) { return isInLoop(); }

template <bool FirstVisit> bool SemaControlFlow::check(DoStmt *D) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

template bool SemaControlFlow::check<true>(DoStmt *);
template bool SemaControlFlow::check<false>(DoStmt *);

template <bool FirstVisit> bool SemaControlFlow::check(ForStmt *F) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

template bool SemaControlFlow::check<true>(ForStmt *);
template bool SemaControlFlow::check<false>(ForStmt *);

template <bool FirstVisit> bool SemaControlFlow::check(ForRangeStmt *F) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

template bool SemaControlFlow::check<true>(ForRangeStmt *);
template bool SemaControlFlow::check<false>(ForRangeStmt *);

bool SemaControlFlow::check(ReturnStmt *R) { return InFunction; }

template <bool FirstVisit> bool SemaControlFlow::check(WhileStmt *W) {
  if constexpr (FirstVisit)
    enterLoop();
  else
    exitLoop();
  return true;
}

template bool SemaControlFlow::check<true>(WhileStmt *);
template bool SemaControlFlow::check<false>(WhileStmt *);