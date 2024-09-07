#pragma once

#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "Exec/IO.h"
#include "Exec/Value.h"
#include "Support/StringMap.h"
#include "Support/Support.h"

#include <cstddef>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace cawk {
class Exec : public ASTVisitor<Exec, trav::None, true> {
  friend class ASTVisitor<Exec, trav::None, true>;
  Diagnostic &Diags;
  TranslationUnitDecl *AST;
  std::vector<Value *> FieldTable;
  std::vector<InputFile> Inputs;
  std::unordered_map<std::string, Value *> Globals;
  std::unordered_set<Value *> InputModifiers;
  std::unordered_set<Value *> OutputModifiers;
  std::vector<CallExpr *> CallStack;
  bool ShouldBreak = false;
  bool ShouldContinue = false;
  bool ShouldReturn = false;
  bool SkipToNext = false;
  bool SkipToNextfile = false;

public:
  Exec(Diagnostic &Diags, TranslationUnitDecl *AST,
       std::vector<InputFile> Inputs,
       std::unordered_map<std::string, Value *> Globals)
      : Diags(Diags), AST(AST), Inputs(Inputs), Globals(Globals) {
    initBuiltinVariables();
  }
  void operator()();

private:
  void addInput(std::string Filepath);

  bool visit(TranslationUnitDecl *T);
  bool visit(RuleDecl *R);

  bool visit(BreakStmt *B);
  bool visit(ContinueStmt *C);
  bool visit(CompoundStmt *C);
  bool visit(DeleteStmt *D);
  bool visit(DoStmt *D);
  bool visit(ExitStmt *E);
  bool visit(ForStmt *F);
  bool visit(ForRangeStmt *F);
  bool visit(IfStmt *I);
  bool visit(NextStmt *N);
  bool visit(NextfileStmt *N);
  bool visit(PrintStmt *P);
  bool visit(ReturnStmt *R);
  bool visit(ValueStmt *V);
  bool visit(WhileStmt *W);

  bool visit(ArraySubscriptExpr *A);
  bool visit(BinaryOperator *B);
  bool visit(CallExpr *C);
  bool visit(DeclRefExpr *D);
  bool visit(FloatingLiteral *F);
  bool visit(RegexLiteral *R);
  bool visit(StringLiteral *S);
  bool visit(UnaryOperator *U);

  static bool isBuiltin(tok::TokenKind Kind);
  bool execBuiltin(tok::TokenKind Kind, std::vector<Value *> Args);
  bool isEarlyExit();
  void updateFields(Value *V);
  void splitFields();
  void joinFields();
  void initBuiltinVariables();
};

} // namespace cawk
