#include "Sema/Sema.h"
#include "AST/ASTPrinter.h"
#include "Sema/SemaControlFlow.h"
#include "Sema/SemaLValue.h"
#include "Sema/SemaType.h"
#include "Sema/SymbolResolver.h"

#include <cstdio>

using namespace cawk;

template <bool FirstVisit> bool Sema::check(FunctionDecl *F) {
  return ControlFlowSema.check<FirstVisit>(F);
}

template bool Sema::check<true>(FunctionDecl *);
template bool Sema::check<false>(FunctionDecl *);

bool Sema::check(BreakStmt *S) { return ControlFlowSema.check(S); }

bool Sema::check(ContinueStmt *S) { return ControlFlowSema.check(S); }

template <bool FirstVisit> bool Sema::check(DoStmt *S) {
  return ControlFlowSema.check<FirstVisit>(S);
}

template bool Sema::check<true>(DoStmt *);
template bool Sema::check<false>(DoStmt *);

template <bool FirstVisit> bool Sema::check(ForStmt *S) {
  return ControlFlowSema.check<FirstVisit>(S);
}

template bool Sema::check<true>(ForStmt *);
template bool Sema::check<false>(ForStmt *);

template <bool FirstVisit> bool Sema::check(ForRangeStmt *S) {
  return ControlFlowSema.check<FirstVisit>(S);
}

template bool Sema::check<true>(ForRangeStmt *);
template bool Sema::check<false>(ForRangeStmt *);

bool Sema::check(ReturnStmt *S) { return ControlFlowSema.check(S); }

template <bool FirstVisit> bool Sema::check(WhileStmt *S) {
  return ControlFlowSema.check<FirstVisit>(S);
}

template bool Sema::check<true>(WhileStmt *);
template bool Sema::check<false>(WhileStmt *);

bool Sema::check(TranslationUnitDecl *T) {
  SemaType TypeSema(Diags);
  SemaLValue LValueSema(Diags);
  SemaControlFlow ControlFlowSema(Diags);
  SymbolResolver Resolver(Diags);
  ASTPrinter Printer;
  bool Result = true;

  Result = Resolver.check(T);
  //   std::printf("SymbolResolver: %s\n", Result ? "pass" : "fail");

  Result = TypeSema.check(T);
  //   std::printf("SemaType: %s\n", Result ? "pass" : "fail");

  Result = LValueSema.check(T);
  //   std::printf("SemaLValue: %s\n", Result ? "pass" : "fail");

  return true;
}