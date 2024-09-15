#pragma once

#include "AST/ASTVisitor.h"

#include <cstdio>
#include <string>
#include <unordered_set>
#include <vector>

namespace cawk {
class ASTDepthFinder : public ASTVisitor<ASTDepthFinder, trav::Preorder, true> {
  std::unordered_map<void *, std::size_t> Table;
  std::size_t Level = 0;

  friend class ASTVisitor<ASTDepthFinder, trav::Preorder, true>;

private:
  bool visit(FunctionDecl *F) {
    for (VarDecl *V : F->getParams())
      Table[V] = Table[F] + 1;

    Table[F->getBody()] = Table[F] + 1;

    return true;
  }

  bool visit(VarDecl *V) {
    Table[V->getDeclRefExpr()] = Table[V] + 1;
    return true;
  }

  bool visit(RuleDecl *R) {
    Table[R->getPattern()] = Table[R->getAction()] = Table[R] + 1;
    return true;
  }

  bool visit(TranslationUnitDecl *T) {
    for (Decl *D : T->getDecls())
      Table[D] = Table[T] + 1;
    return true;
  }

  bool visit(BreakStmt *B) { return true; }

  bool visit(ContinueStmt *C) { return true; }

  bool visit(CompoundStmt *C) {
    for (Stmt *S : C->getBody())
      Table[S] = Table[C] + 1;
    return true;
  }

  bool visit(DeleteStmt *D) { return true; }

  bool visit(DoStmt *D) {
    Table[D->getCond()] = Table[D->getBody()] = Table[D] + 1;
    return true;
  }

  bool visit(ExitStmt *E) {
    Table[E->getValue()] = Table[E] + 1;
    return true;
  }

  bool visit(ForStmt *F) {
    Table[F->getInit()] = Table[F->getCond()] = Table[F->getInc()] =
        Table[F->getBody()] = Table[F] + 1;
    return true;
  }

  bool visit(ForRangeStmt *F) {
    Table[F->getLoopVar()] = Table[F->getRange()] = Table[F->getBody()] =
        Table[F] + 1;
    return true;
  }

  bool visit(IfStmt *I) {
    Table[I->getCond()] = Table[I->getThen()] = Table[I->getElse()] =
        Table[I] + 1;
    return true;
  }

  bool visit(NextStmt *N) { return true; }

  bool visit(NextfileStmt *N) { return true; }

  bool visit(PrintStmt *P) {
    for (Expr *E : P->getArgs())
      Table[E] = Table[P] + 1;
    return true;
  }

  bool visit(ReturnStmt *R) {
    Table[R->getValue()] = Table[R] + 1;
    return true;
  }

  bool visit(ValueStmt *V) {
    Table[V->getValue()] = Table[V] + 1;
    return true;
  }

  bool visit(WhileStmt *W) {
    Table[W->getCond()] = Table[W->getBody()] = Table[W] + 1;
    return true;
  }

  bool visit(ArraySubscriptExpr *A) {
    Table[A->getLHS()] = Table[A] + 1;
    for (Expr *E : A->getRHS())
      Table[E] = Table[A] + 1;
    return true;
  }

  bool visit(BinaryOperator *B) {
    Table[B->getLHS()] = Table[B->getRHS()] = Table[B] + 1;
    return true;
  }

  bool visit(CallExpr *C) {
    Table[C->getCallee()] = Table[C] + 1;
    for (Expr *E : C->getArgs())
      Table[E] = Table[C] + 1;
    return true;
  }

  bool visit(DeclRefExpr *D) { return true; }

  bool visit(FloatingLiteral *F) { return true; }

  bool visit(LambdaExpr *L) {
    for (VarDecl *V : L->getParams())
      Table[V] = Table[L] + 1;

    Table[L->getBody()] = Table[L] + 1;
    return true;
  }

  bool visit(RegexLiteral *R) { return true; }

  bool visit(StringLiteral *S) { return true; }

  bool visit(UnaryOperator *U) {
    Table[U->getSubExpr()] = Table[U] + 1;
    return true;
  }

public:
  std::unordered_map<void *, std::size_t> getTable() { return Table; }

  std::unordered_map<void *, std::size_t> getDepths(auto *P) {
    Table[P] = 0;
    traverse(P);
    return getTable();
  }
};
}; // namespace cawk