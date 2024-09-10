#pragma once

#include "AST/ASTDepthFinder.h"
#include "AST/ASTVisitor.h"

#include <cstdio>
#include <string>
#include <unordered_set>
#include <vector>

namespace cawk {
class ASTPrinter : public ASTVisitor<ASTPrinter, trav::Preorder, true> {
  std::unordered_map<void *, std::size_t> Table;

  friend class ASTVisitor<ASTPrinter, trav::Preorder, true>;

private:
  void print(std::string Name, void *P, auto &&...Text) {
    std::string S = std::string(Table[P] * 2, ' ') + Name + toString(P) +
                    (std::string() + ... + std::string(Text));
    std::puts(S.c_str());
  }

  std::string toString(void *P) {
    std::string Buffer(20, ' ');
    Buffer.resize(std::snprintf(Buffer.data(), std::size(Buffer), "%p", P));
    return Buffer;
  }

  bool visit(FunctionDecl *F) {
    print(std::string("FunctionDecl "), F, " ", F->getName());
    return true;
  }
  
  bool visit(RuleDecl *R) {
    print(std::string("RuleDecl "), R);
    return true;
  }

  bool visit(TranslationUnitDecl *T) {
    print(std::string("TranslationUnitDecl "), T);
    return true;
  }

  bool visit(BreakStmt *B) {
    print(std::string("BreakStmt "), B);
    return true;
  }

  bool visit(ContinueStmt *C) {
    print(std::string("ContinueStmt "), C);
    return true;
  }

  bool visit(CompoundStmt *C) {
    print(std::string("CompoundStmt "), C);
    return true;
  }

  bool visit(DeleteStmt *D) {
    print(std::string("DeleteStmt "), D);
    return true;
  }

  bool visit(DoStmt *D) {
    print(std::string("DoStmt "), D);
    return true;
  }

  bool visit(ExitStmt *E) {
    print(std::string("ExitStmt "), E);
    return true;
  }

  bool visit(ForStmt *F) {
    print(std::string("ForStmt "), F);
    return true;
  }

  bool visit(ForRangeStmt *F) {
    print(std::string("ForRangeStmt "), F);
    return true;
  }

  bool visit(IfStmt *I) {
    print(std::string("IfStmt "), I);
    return true;
  }

  bool visit(NextStmt *N) {
    print(std::string("NextStmt "), N);
    return true;
  }

  bool visit(NextfileStmt *N) {
    print(std::string("Nextfile "), N);
    return true;
  }

  bool visit(PrintStmt *P) {
    print(std::string("PrintStmt "), P);
    return true;
  }

  bool visit(ReturnStmt *R) {
    print(std::string("ReturnStmt "), R);
    return true;
  }

  bool visit(ValueStmt *V) {
    print(std::string("ValueStmt "), V);
    return true;
  }

  bool visit(WhileStmt *W) {
    print(std::string("WhileStmt "), W);
    return true;
  }

  bool visit(ArraySubscriptExpr *A) {
    print(std::string("ArraySubscriptExpr "), A);
    return true;
  }

  bool visit(BinaryOperator *B) {
    print(std::string("BinaryOperator "), B);
    return true;
  }

  bool visit(CallExpr *C) {
    print(std::string("CallExpr "), C);
    return true;
  }

  bool visit(DeclRefExpr *D) {
    print(std::string("DeclRefExpr "), D, " ", D->getName());
    return true;
  }

  bool visit(FloatingLiteral *F) {
    print(std::string("FloatingLiteral "), F, " ",
          F->getLiteral().getLiteralData());
    return true;
  }

  bool visit(RegexLiteral *R) {
    print(std::string("RegexLiteral "), R, " ", R->getLiteral().getLiteralData());
    return true;
  }

  bool visit(StringLiteral *S) {
    print(std::string("StringLiteral "), S, " ",
          S->getLiteral().getLiteralData());
    return true;
  }

  bool visit(UnaryOperator *U) {
    print(std::string("UnaryOperator "), U);
    return true;
  }

public:
  bool traverse(auto *P) {
    Table = ASTDepthFinder().getDepths(P);
    return ASTVisitor<ASTPrinter, trav::Preorder, true>::visit(P);
  }
};
}; // namespace cawk