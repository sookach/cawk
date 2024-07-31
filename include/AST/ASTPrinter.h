#pragma once

#include "AST/ASTVisitor.h"

#include <cstdio>
#include <string>
#include <vector>

namespace cawk {
class ASTPrinter : public ASTVisitor<ASTPrinter, trav::RecursiveDescent> {
  std::vector<void *> Stack;

private:
  void print(std::string Text) {
    std::string S(std::size(Text), ' ');
    S += Text;
    std::puts(S.c_str());
  }

  std::string toString(void *P) {
    std::string Buffer(20, ' ');
    Buffer.resize(std::snprintf(Buffer.data(), std::size(Buffer), "%p", P));
    return Buffer;
  }

  bool traverse(void *P) {
    if (std::empty(Stack) || Stack.back() != P) {
      Stack.push_back(P);
      return true;
    }
    Stack.pop_back();
    return false;
  }

public:
  bool visit(FunctionDecl *F) {
    if (traverse(F))
      print(std::string("FunctionDecl ") + toString(F));

    return true;
  }

  bool visit(ParamVarDecl *P) {
    if (traverse(P))
      print(std::string("ParamVarDecl ") + toString(P));
    return true;
  }

  bool visit(RuleDecl *R) {
    if (traverse(R))
      print(std::string("RuleDecl ") + toString(R));
    return true;
  }

  bool visit(TranslationUnitDecl *T) {
    if (traverse(T))
      print(std::string("TranslationUnitDecl ") + toString(T));
    return true;
  }

  bool visit(BreakStmt *B) {
    if (traverse(B))
      print(std::string("BreakStmt ") + toString(B));
    return true;
  }

  bool visit(ContinueStmt *C) {
    if (traverse(C))
      print(std::string("ContinueStmt ") + toString(C));
    return true;
  }

  bool visit(CompoundStmt *C) {
    if (traverse(C))
      print(std::string("CompoundStmt ") + toString(C));
    return true;
  }

  bool visit(DeleteStmt *D) {
    if (traverse(D))
      print(std::string("DeleteStmt ") + toString(D));
    return true;
  }

  bool visit(DoStmt *D) {
    if (traverse(D))
      print(std::string("DoStmt ") + toString(D));
    return true;
  }

  bool visit(ExitStmt *E) {
    if (traverse(E))
      print(std::string("ExitStmt ") + toString(E));
    return true;
  }

  bool visit(ForStmt *F) {
    if (traverse(F))
      print(std::string("ForStmt ") + toString(F));
    return true;
  }

  bool visit(ForRangeStmt *F) {
    if (traverse(F))
      print(std::string("ForRangeStmt ") + toString(F));
    return true;
  }

  bool visit(IfStmt *I) {
    if (traverse(I))
      print(std::string("IfStmt ") + toString(I));
    return true;
  }

  bool visit(NextStmt *N) {
    if (traverse(N))
      print(std::string("NextStmt ") + toString(N));
    return true;
  }

  bool visit(NextfileStmt *N) {
    if (traverse(N))
      print(std::string("Nextfile ") + toString(N));
    return true;
  }

  bool visit(PrintStmt *P) {
    if (traverse(P))
      print(std::string("PrintStnt ") + toString(P));
    return true;
  }

  bool visit(ReturnStmt *R) {
    if (traverse(R))
      print(std::string("ReturnStmt ") + toString(R));
    return true;
  }

  bool visit(ValueStmt *V) {
    if (traverse(V))
      print(std::string("ValueStmt ") + toString(V));
    return true;
  }

  bool visit(WhileStmt *W) {
    if (traverse(W))
      print(std::string("WhileStmt ") + toString(W));
    return true;
  }

  bool visit(ArraySubscriptExpr *A) {
    if (traverse(A))
      print(std::string("ArraySubscriptExpr ") + toString(A));
    return true;
  }

  bool visit(BinaryOperator *B) {
    if (traverse(B))
      print(std::string("BinaryOperator ") + toString(B));
    return true;
  }

  bool visit(CallExpr *C) {
    if (traverse(C))
      print(std::string("CallExpr ") + toString(C));
    return true;
  }

  bool visit(DeclRefExpr *D) {
    if (traverse(D))
      print(std::string("DeclRefExpr ") + toString(D));
    return true;
  }

  bool visit(FloatingLiteral *F) {
    if (traverse(F))
      print(std::string("FloatingLiteral ") + toString(F));
    return true;
  }

  bool visit(RegexLiteral *R) {
    if (traverse(R))
      print(std::string("RegexLiteral ") + toString(R));
    return true;
  }

  bool visit(StringLiteral *S) {
    if (traverse(S))
      print(std::string("StringLiteral ") + toString(S));
    return true;
  }

  bool visit(UnaryOperator *U) {
    if (traverse(U))
      print(std::string("UnaryOperator ") + toString(U));
    return true;
  }
};
}; // namespace cawk