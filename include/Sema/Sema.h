#pragma once

#include "AST/AST.h"
#include "Basic/Diagnostic.h"
#include "Sema/SemaControlFlow.h"
#include "Sema/SemaType.h"

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
    bool contains(std::string Name) {
      return Globals.contains(Name) || Locals.contains(Name);
    }

    Value *at(std::string Name) {
      if (Locals.contains(Name))
        return Locals.at(Name);
      return Globals.at(Name);
    }

    void addLocal(std::string Name, Value *V) { Locals[Name] = V; }
    void addGlobal(std::string Name, Value *V) { Globals[Name] = V; }

    void resolve(std::string Name, Expr *E) {
      if (contains(Name))
        E->setValue(at(Name));
      else
        Globals[Name] = E->getValue();
    }
  };

  std::vector<std::unordered_set<TypeKind>> ReturnTypes;

  Diagnostic &Diags;
  ControlFlow CtrlFlow;
  SymbolTable Symbols;

public:
  Sema(Diagnostic &Diags) : Diags(Diags) {}

  bool check(TranslationUnitDecl *T);

  template <bool FirstVisit> bool check(FunctionDecl *F);
  void start(FunctionDecl *F);
  void check(FunctionDecl *F);

  bool check(BreakStmt *S);
  bool check(ContinueStmt *S);
  template <bool FirstVisit> StmtResult check(DoStmt *S);
  template <bool FirstVisit> StmtResult check(ForStmt *S);
  template <bool FirstVisit> StmtResult check(ForRangeStmt *S);
  StmtResult check(PrintStmt *S);
  StmtResult check(ReturnStmt *S);
  template <bool FirstVisit> bool check(WhileStmt *S);
};

} // namespace cawk