#pragma once

#include "AST/AST.h"
#include "Basic/Diagnostic.h"

namespace cawk {
class Sema {
  class ControlFlow {
    std::uint16_t LoopDepth = 0;
    std::uint16_t FunctionDepth = 0;

  public:
    bool isInLoop() { return LoopDepth != 0; }
    bool isInFunction() { return FunctionDepth != 0; }
    void enterLoop() { ++LoopDepth; }
    void exitLoop() { --LoopDepth; }
    void enterFunction() { ++FunctionDepth; }
    void exitFunction() { --FunctionDepth; }
  };

  Diagnostic &Diags;
  ControlFlow CtrlFlow;
  std::unordered_map<std::string, Value *> Symbols;

public:
  Sema(Diagnostic &Diags) : Diags(Diags) {}

  bool actOnParamList(std::vector<ParamVarDecl *> Params);
  void actOnStartOfFunctionBody();
  void actOnFinishOfFunctionBody();
  DeclResult actOnFunctionDeclaration(FunctionDecl *F);
  StmtResult actOnBreakStatement(BreakStmt *B);
  StmtResult actOnContinueStatement(ContinueStmt *C);
  void actOnStartOfDoStatement();
  void actOnFinishOfDoStatement();
  void actOnStartOfForStatement();
  void actOnFinishOfForStatement();
  StmtResult actOnReturnStatement(ReturnStmt *R);
  void actOnStartOfWhileStatement();
  void actOnFinishOfWhileStatement();
  bool actOnDeclRefExpr(DeclRefExpr *D);

  auto getSymbols() { return Symbols; }
};

} // namespace cawk