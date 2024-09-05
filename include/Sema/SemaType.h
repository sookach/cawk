#pragma once

#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "Basic/Diagnostic.h"
#include "Exec/IO.h"

namespace cawk {
class SemaType : ASTVisitor<SemaType, trav::Postorder, true> {
  friend class ASTVisitor<SemaType, trav::Postorder, true>;
  Diagnostic &Diags;

public:
  SemaType(Diagnostic &Diags) : Diags(Diags) {}
  bool check(TranslationUnitDecl *T);
  bool checkType(TypeKind T1, TypeKind T2);
  bool checkType(Expr *E, TypeKind T);

private:
  bool visit(RuleDecl *R);

  bool visit(DeleteStmt *D);
  bool visit(DoStmt *D);
  bool visit(ExitStmt *E);
  bool visit(ForStmt *F);
  bool visit(ForRangeStmt *F);
  bool visit(IfStmt *I);
  bool visit(PrintStmt *P);
  bool visit(ReturnStmt *R);
  bool visit(WhileStmt *W);

  bool visit(ArraySubscriptExpr *A);
  bool visit(BinaryOperator *B);
  bool visit(CallExpr *C);
  bool visit(FloatingLiteral *F);
  bool visit(RegexLiteral *R);
  bool visit(StringLiteral *S);
  bool visit(UnaryOperator *U);
};

} // namespace cawk