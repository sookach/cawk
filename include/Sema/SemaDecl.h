#pragma once

#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "Support/StringMap.h"

#include <string_view>
#include <unordered_map>

namespace cawk {
class SemaDecl : ASTVisitor<SemaDecl, trav::Preorder, true> {
  friend class ASTVisitor<SemaDecl, trav::Preorder, true>;

  std::unordered_map<std::string_view, FunctionDecl *> FunctionMap;
  std::vector<FunctionDecl *> FunctionDecls;
  std::vector<RuleDecl *> RuleDecls;
  std::unordered_map<std::string, std::variant<FunctionDecl *, DeclRefExpr *>>
      GlobalRefs;
  std::unordered_map<std::string, ParamVarDecl *> LocalRefs;

public:
  bool check(TranslationUnitDecl *T);
  std::unordered_map<std::string_view, FunctionDecl *> getFunctionMap();
  std::vector<FunctionDecl *> getFunctionDecls();
  std::vector<RuleDecl *> getRuleDecls();

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