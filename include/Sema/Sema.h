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
    std::unordered_map<std::string, Value *> Globals;
    std::unordered_map<std::string, Value *> Locals;

  public:
    bool containsLocal(std::string Name) { return Locals.contains(Name); }
    bool containsGlobal(std::string Name) { return Globals.contains(Name); }
    bool contains(std::string Name) {
      return containsLocal(Name) || containsGlobal(Name);
    }

    Value *at(std::string Name) {
      if (Locals.contains(Name))
        return Locals.at(Name);
      return Globals.at(Name);
    }

    void clearLocals() { Locals.clear(); }
    void addLocal(std::string Name, Value *V) { Locals[Name] = V; }
    void addGlobal(std::string Name, Value *V) { Globals[Name] = V; }
    auto getGlobals() { return Globals; }

    void resolve(std::string Name, Expr *E) {
      if (contains(Name))
        E->setValue(at(Name));
      else
        Globals[Name] = E->getValue();
    }
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
  void actOnDeclRefExpr(DeclRefExpr *D);

  auto getSymbols() { return Symbols; }
};

} // namespace cawk