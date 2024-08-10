#pragma once

#include "AST/AST.h"
#include "AST/ASTVisitor.h"

#include <vector>

namespace cawk {
class SemaControlFlow
    : public ASTVisitor<SemaControlFlow, trav::RecursiveDescent, true> {
  friend class ASTVisitor<SemaControlFlow, trav::RecursiveDescent, true>;

private:
  std::uint16_t LoopDepth = 0;
  bool InFunction = false;

  bool isInLoop() { return LoopDepth != 0; }

  void enterLoop() { ++LoopDepth; }

  void exitLoop() { --LoopDepth; }

public:
  bool check(TranslationUnitDecl *T);

private:
  template <bool FirstVisit> bool visit(Decl *D);
  template <bool FirstVisit> bool visit(FunctionDecl *F);
  template <bool FirstVisit> bool visit(ParamVarDecl *P);
  template <bool FirstVisit> bool visit(RuleDecl *R);
  template <bool FirstVisit> bool visit(TranslationUnitDecl *T);
  template <bool FirstVisit> bool visit(VarDecl *V);

  template <bool FirstVisit> bool visit(Stmt *S);
  template <bool FirstVisit> bool visit(BreakStmt *B);
  template <bool FirstVisit> bool visit(CompoundStmt *C);
  template <bool FirstVisit> bool visit(ContinueStmt *C);
  template <bool FirstVisit> bool visit(DeleteStmt *D);
  template <bool FirstVisit> bool visit(DoStmt *D);
  template <bool FirstVisit> bool visit(ExitStmt *E);
  template <bool FirstVisit> bool visit(ForStmt *F);
  template <bool FirstVisit> bool visit(ForRangeStmt *F);
  template <bool FirstVisit> bool visit(IfStmt *I);
  template <bool FirstVisit> bool visit(NextStmt *N);
  template <bool FirstVisit> bool visit(NextfileStmt *N);
  template <bool FirstVisit> bool visit(PrintStmt *P);
  template <bool FirstVisit> bool visit(ReturnStmt *R);
  template <bool FirstVisit> bool visit(ValueStmt *V);
  template <bool FirstVisit> bool visit(WhileStmt *W);

  template <bool FirstVisit> bool visit(Expr *E);
  template <bool FirstVisit> bool visit(ArraySubscriptExpr *A);
  template <bool FirstVisit> bool visit(BinaryOperator *B);
  template <bool FirstVisit> bool visit(CallExpr *C);
  template <bool FirstVisit> bool visit(DeclRefExpr *D);
  template <bool FirstVisit> bool visit(FloatingLiteral *F);
  template <bool FirstVisit> bool visit(RegexLiteral *R);
  template <bool FirstVisit> bool visit(StringLiteral *S);
  template <bool FirstVisit> bool visit(UnaryOperator *U);
};
} // namespace cawk