#include "Sema/Sema.h"
#include "AST/ASTPrinter.h"
#include "Support/Support.h"

#include <cstdio>

using namespace cawk;

bool Sema::actOnParamList(std::vector<ParamVarDecl *> Params) {
  std::unordered_set<std::string> Names;
  for (ParamVarDecl *P : Params)
    if (!Names.emplace(P->getName()).second)
      return false;
  return true;
}

void Sema::actOnStartOfFunctionBody() { CtrlFlow.enterFunction(); }

void Sema::actOnFinishOfFunctionBody() { CtrlFlow.exitFunction(); }

DeclResult Sema::actOnFunctionDeclaration(FunctionDecl *F) {
  if (Symbols.emplace(F->getName(), new Value(F)).second)
    return F;
  return false;
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