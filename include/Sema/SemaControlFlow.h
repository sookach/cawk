#pragma once

#include "AST/AST.h"
#include "Basic/Diagnostic.h"

namespace cawk {
class SemaControlFlow {

private:
  Diagnostic &Diags;
  std::uint16_t LoopDepth = 0;
  bool InFunction = false;

  bool isInLoop() { return LoopDepth != 0; }

  void enterLoop() { ++LoopDepth; }

  void exitLoop() { --LoopDepth; }

public:
  SemaControlFlow(Diagnostic &Diags) : Diags(Diags) {}

  template <bool FirstVisit> bool check(FunctionDecl *F);

  bool check(BreakStmt *B);
  bool check(ContinueStmt *C);
  template <bool FirstVisit> bool check(DoStmt *D);
  template <bool FirstVisit> bool check(ForStmt *F);
  template <bool FirstVisit> bool check(ForRangeStmt *F);
  bool check(ReturnStmt *R);
  template <bool FirstVisit> bool check(WhileStmt *W);
};
} // namespace cawk