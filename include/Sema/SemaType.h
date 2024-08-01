#pragma once

#include "AST/AST.h"
#include "Sema/FunctionSymbol.h"
#include "Support/StringMap.h"
#include "Sema/Type.h"

#include <vector>

namespace cawk {
class SemaType {
  StringMap<std::vector<TypeKind>> FunctionPrototypes;
  StringMap<TypeKind> Globals;
  StringMap<TypeKind> Locals;

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

  TypeKind visit(Expr *E);
  TypeKind visit(ArraySubscriptExpr *A);
  TypeKind visit(BinaryOperator *B);
  TypeKind visit(CallExpr *C);
  TypeKind visit(DeclRefExpr *D);
  TypeKind visit(FloatingLiteral *F);
  TypeKind visit(RegexLiteral *R);
  TypeKind visit(StringLiteral *S);
  TypeKind visit(UnaryOperator *U);

  TypeKind getType(std::string_view S) {
    return Locals.contains(S)    ? Locals.at(S)
           : Globals.contains(S) ? Globals.at(S)
                                 : TK_Null;
  }

  void setType(std::string_view S, TypeKind T) {
    if (Locals.contains(S))
      Locals[S] = T;
    Globals[S] = T;
  }
};
} // namespace cawk