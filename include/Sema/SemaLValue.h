#pragma once

#include "AST/ASTVisitor.h"
#include "Sema/FunctionSymbol.h"
#include "Support/StringMap.h"

#include <vector>

namespace cawk {
class SemaLValue : public ASTVisitor<SemaLValue, trav::Preorder, true> {
  friend class ASTVisitor<SemaLValue, trav::Preorder, true>;

public:
  bool check(TranslationUnitDecl *T);

private:
  bool visit(Decl *D);
  bool visit(FunctionDecl *F);
  bool visit(ParamVarDecl *P);
  bool visit(RuleDecl *R);
  bool visit(TranslationUnitDecl *T);
  bool visit(VarDecl *V);

  bool visit(Stmt *S);
  bool visit(BreakStmt *B);
  bool visit(CompoundStmt *C);
  bool visit(ContinueStmt *C);
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

  bool visit(Expr *E);
  bool visit(ArraySubscriptExpr *A);
  bool visit(BinaryOperator *B);
  bool visit(CallExpr *C);
  bool visit(DeclRefExpr *D);
  bool visit(FloatingLiteral *F);
  bool visit(RegexLiteral *R);
  bool visit(StringLiteral *S);
  bool visit(UnaryOperator *U);
};
} // namespace cawk