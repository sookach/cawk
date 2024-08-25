#pragma once

#include "AST/AST.h"
#include "Basic/Diagnostic.h"
#include "Sema/SemaControlFlow.h"

namespace cawk {
class Sema {
  Diagnostic &Diags;
  SemaControlFlow ControlFlowSema;

public:
  Sema(Diagnostic &Diags) : Diags(Diags), ControlFlowSema(Diags) {}

  bool check(TranslationUnitDecl *T);

  template <bool FirstVisit> bool check(FunctionDecl *F);

  bool check(BreakStmt *S);
  bool check(ContinueStmt *S);
  template <bool FirstVisit> bool check(DoStmt *S);
  template <bool FirstVisit> bool check(ForStmt *S);
  template <bool FirstVisit> bool check(ForRangeStmt *S);
  bool check(ReturnStmt *S);
  template <bool FirstVisit> bool check(WhileStmt *S);
};

} // namespace cawk