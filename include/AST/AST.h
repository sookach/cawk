#pragma once

#include <vector>

class Decl;
class TranslationUnitDecl;
class RuleDecl;
class FunctionDecl;
class VarDecl;

class Stmt;
class ForStmt;
class IfStmt;
class WhileStmt;
class DoStmt;
class DeclStmt;
class ExprStmt;
class CompoundStmt;
class ValueStmt;
class Expr;
class BinaryExpr;
class CallExpr;
class DeclRefExpr;
class FloatingLiteral;
class StringLiteral;
class UnaryExpr;

class Decl {
public:
  enum DeclKind { DK_TranslationUnit, DK_Rule, DK_Function, DK_Var };

  Decl(DeclKind K) : Kind(K) {}

  DeclKind GetKind() const { return Kind; }

private:
  const DeclKind Kind;
};

class TranslationUnitDecl : public Decl {
  std::vector<Decl *> Decls;

public:
  TranslationUnitDecl() : Decl(DK_TranslationUnit) {}
  TranslationUnitDecl(std::vector<Decl *> &D)
      : Decl(DK_TranslationUnit), Decls(D) {}

  const std::vector<Decl *> &GetDecls() const { return Decls; }
};

class FunctionDecl : public Decl {
  std::string Name;
  std::vector<std::string> Params;
  CompoundStmt *Body;

  FunctionDecl() : Decl(DK_Function) {}

  FunctionDecl(std::string N, std::vector<std::string> P, CompoundStmt *B)
      : Decl(DK_Function), Name(N), Params(P), Body(B) {}

public:
  static FunctionDecl *Create(std::string N, std::vector<std::string> P,
                              CompoundStmt *B) {
    return new FunctionDecl(N, P, B);
  }

  static FunctionDecl *CreateEmpty() { return new FunctionDecl; }
};

class RuleDecl : public Decl {
  Expr *Pattern;
  CompoundStmt *Action;

protected:
  RuleDecl() : Decl(DK_Rule) {}

  RuleDecl(Expr *P, CompoundStmt *A) : Decl(DK_Rule), Pattern(P), Action(A) {}

public:
  static RuleDecl *Create(Expr *Pattern, CompoundStmt *Action) {
    return new RuleDecl(Pattern, Action);
  }
};

class Stmt {
public:
  enum StmtKind {
    SK_For,
    SK_If,
    SK_While,
    SK_Do,
    SK_Decl,
    SK_Expr,
    SK_Compound
  };

private:
  const StmtKind Kind;

protected:
  Stmt(StmtKind K) : Kind(K) {}

public:
  StmtKind GetKind() const { return Kind; }
};

class CompoundStmt : public Stmt {
  std::vector<Stmt *> Body{};

protected:
  CompoundStmt() : Stmt(SK_Compound) {}

  CompoundStmt(std::vector<Stmt *> B) : Stmt(SK_Compound), Body(B) {}

public:
  static CompoundStmt *Create(std::vector<Stmt *> B) {
    return new CompoundStmt(B);
  }

  static CompoundStmt *CreateEmpty() { return new CompoundStmt; }

  std::vector<Stmt *> GetBody() const { return Body; }
};
