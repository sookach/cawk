#pragma once

#include "AST/ASTVisitor.h"

#include <cstdio>
#include <string>
#include <unordered_set>
#include <vector>

namespace cawk {
class ASTPrinter : public ASTVisitor<ASTPrinter, trav::RecursiveDescent, true> {
  std::unordered_set<void *> Table;
  std::size_t Offset = 0;

  friend class ASTVisitor<ASTPrinter, trav::RecursiveDescent, true>;

private:
  void print(std::string Text) {
    std::string S(Offset, ' ');
    S += Text;
    std::puts(S.c_str());
  }

  std::string toString(void *P) {
    std::string Buffer(20, ' ');
    Buffer.resize(std::snprintf(Buffer.data(), std::size(Buffer), "%p", P));
    return Buffer;
  }

  bool enter(void *P) {
    if (!Table.contains(P)) {
      Table.insert(P);
      ++Offset;
      return true;
    }
    Table.erase(P);
    --Offset;
    return false;
  }

  bool visit(FunctionDecl *F) {
    if (enter(F))
      print(std::string("FunctionDecl ") + toString(F));

    return true;
  }

  bool visit(ParamVarDecl *P) {
    if (enter(P))
      print(std::string("ParamVarDecl ") + toString(P));
    return true;
  }

  bool visit(RuleDecl *R) {
    if (enter(R))
      print(std::string("RuleDecl ") + toString(R));
    return true;
  }

  bool visit(TranslationUnitDecl *T) {
    if (enter(T))
      print(std::string("TranslationUnitDecl ") + toString(T));
    return true;
  }

  bool visit(BreakStmt *B) {
    if (enter(B))
      print(std::string("BreakStmt ") + toString(B));
    return true;
  }

  bool visit(ContinueStmt *C) {
    if (enter(C))
      print(std::string("ContinueStmt ") + toString(C));
    return true;
  }

  bool visit(CompoundStmt *C) {
    if (enter(C))
      print(std::string("CompoundStmt ") + toString(C));
    return true;
  }

  bool visit(DeleteStmt *D) {
    if (enter(D))
      print(std::string("DeleteStmt ") + toString(D));
    return true;
  }

  bool visit(DoStmt *D) {
    if (enter(D))
      print(std::string("DoStmt ") + toString(D));
    return true;
  }

  bool visit(ExitStmt *E) {
    if (enter(E))
      print(std::string("ExitStmt ") + toString(E));
    return true;
  }

  bool visit(ForStmt *F) {
    if (enter(F))
      print(std::string("ForStmt ") + toString(F));
    return true;
  }

  bool visit(ForRangeStmt *F) {
    if (enter(F))
      print(std::string("ForRangeStmt ") + toString(F));
    return true;
  }

  bool visit(IfStmt *I) {
    if (enter(I))
      print(std::string("IfStmt ") + toString(I));
    return true;
  }

  bool visit(NextStmt *N) {
    if (enter(N))
      print(std::string("NextStmt ") + toString(N));
    return true;
  }

  bool visit(NextfileStmt *N) {
    if (enter(N))
      print(std::string("Nextfile ") + toString(N));
    return true;
  }

  bool visit(PrintStmt *P) {
    if (enter(P))
      print(std::string("PrintStnt ") + toString(P));
    return true;
  }

  bool visit(ReturnStmt *R) {
    if (enter(R))
      print(std::string("ReturnStmt ") + toString(R));
    return true;
  }

  bool visit(ValueStmt *V) {
    if (enter(V))
      print(std::string("ValueStmt ") + toString(V));
    return true;
  }

  bool visit(WhileStmt *W) {
    if (enter(W))
      print(std::string("WhileStmt ") + toString(W));
    return true;
  }

  bool visit(ArraySubscriptExpr *A) {
    if (enter(A))
      print(std::string("ArraySubscriptExpr ") + toString(A));
    return true;
  }

  bool visit(BinaryOperator *B) {
    if (enter(B))
      print(std::string("BinaryOperator ") + toString(B));
    return true;
  }

  bool visit(CallExpr *C) {
    if (enter(C))
      print(std::string("CallExpr ") + toString(C));
    return true;
  }

  bool visit(DeclRefExpr *D) {
    if (enter(D))
      print(std::string("DeclRefExpr ") + toString(D));
    return true;
  }

  bool visit(FloatingLiteral *F) {
    if (enter(F))
      print(std::string("FloatingLiteral ") + toString(F));
    return true;
  }

  bool visit(RegexLiteral *R) {
    if (enter(R))
      print(std::string("RegexLiteral ") + toString(R));
    return true;
  }

  bool visit(StringLiteral *S) {
    if (enter(S))
      print(std::string("StringLiteral ") + toString(S));
    return true;
  }

  bool visit(UnaryOperator *U) {
    if (enter(U))
      print(std::string("UnaryOperator ") + toString(U));
    return true;
  }

public:
  bool traverse(auto *P) {
    return ASTVisitor<ASTPrinter, trav::RecursiveDescent, true>::visit(P);
  }
};
}; // namespace cawk