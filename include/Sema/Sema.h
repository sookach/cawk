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

  class SymbolTable {
    std::vector<std::unordered_map<std::string, Value *>> Tables;
    std::unordered_map<std::string, std::vector<Expr *>> Undefined;

  public:
    void addTable() { Tables.emplace_back(); }
    void removeTable() { Tables.pop_back(); }
    Value *lookup(std::string Name) {
      for (auto It = std::rbegin(Tables); It != std::rend(Tables); ++It)
        if (It->contains(Name))
          return It->at(Name);
      return nullptr;
    }
    bool contains(std::string Name) {
      for (auto It = std::rbegin(Tables); It != std::rend(Tables); ++It)
        if (It->contains(Name))
          return true;
      return false;
    }
    auto getCurrentTable() { return Tables.back(); }
    bool alreadyDefined(std::string Name) {
      return Tables.back().contains(Name);
    }
    void addUndefined(std::string Name, Value *V) {
      Undefined[Name].push_back(V);
    }
    void removeUndefined(std::string Name) { Undefined.erase(Name); }
  };

  Diagnostic &Diags;
  ControlFlow CtrlFlow;
  SymbolTable Symbols;

public:
  Sema(Diagnostic &Diags) : Diags(Diags) {}

  bool actOnParamList(std::vector<ParamVarDecl *> Params);
  void actOnStartOfFunctionBody();
  void actOnFinishOfFunctionBody();
  StmtResult actOnBreakStatement(BreakStmt *B);
  StmtResult actOnContinueStatement(ContinueStmt *C);
  void actOnStartOfDoStatement();
  void actOnFinishOfDoStatement();
  void actOnStartOfForStatement();
  void actOnFinishOfForStatement();
  StmtResult actOnReturnStatement(ReturnStmt *R);
  void actOnStartOfWhileStatement();
  void actOnFinishOfWhileStatement();
  void actOnCallExpr(CallExpr *C);
  void actOnDeclRefExpr(DeclRefExpr *D);

  auto getSymbols() { return Symbols; }
};

} // namespace cawk