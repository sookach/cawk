#include "Sema/Sema.h"
#include "AST/ASTPrinter.h"
#include "Sema/SemaControlFlow.h"
#include "Sema/SemaLValue.h"
#include "Sema/SemaType.h"
#include "Sema/SymbolResolver.h"

#include <cstdio>

using namespace cawk;

bool Sema::actOnParamList(std::vector<ParamVarDecl *> Params) {
  Symbols.clearLocals();
  for (ParamVarDecl *P : Params) {
    if (Symbols.containsLocal(std::string(P->getName()))) {
      return false;
    }
    Symbols.addLocal(std::string(P->getName()), new Value);
  }
  return true;
}

void Sema::actOnStartOfFunctionBody() { CtrlFlow.enterFunction(); }

void Sema::actOnFinishOfFunctionBody() {
  CtrlFlow.exitFunction();
  Symbols.clearLocals();
}

StmtResult Sema::actOnBreakStatement(BreakStmt *B) {
  if (!CtrlFlow.isInLoop())
    return false;
  return B;
}

StmtResult Sema::actOnContinueStatement(ContinueStmt *C) {
  if (!CtrlFlow.isInLoop())
    return false;
  return C;
}

void Sema::actOnStartOfDoStatement() { CtrlFlow.enterLoop(); }

void Sema::actOnFinishOfDoStatement() { CtrlFlow.exitLoop(); }

void Sema::actOnStartOfForStatement() { CtrlFlow.enterLoop(); }

void Sema::actOnFinishOfForStatement() { CtrlFlow.exitLoop(); }

StmtResult Sema::actOnReturnStatement(ReturnStmt *R) {
  if (!CtrlFlow.isInFunction())
    return false;
  return R;
}

void Sema::actOnStartOfWhileStatement() { CtrlFlow.enterLoop(); }

void Sema::actOnFinishOfWhileStatement() { CtrlFlow.exitLoop(); }

void Sema::actOnDeclRefExpr(DeclRefExpr *D) {
  Symbols.resolve(std::string(D->getName()), D);
}