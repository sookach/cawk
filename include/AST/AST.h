#pragma once

#include <vector>

class Decl;
class TranslationUnitDecl;
class PatternActionDecl;
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
  enum DeclKind { DK_TranslationUnit, DK_PatternAction, DK_Function, DK_Var };

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
  CompoundStmt *Body;
};

class PatternActionDecl : public Decl {
  Expr *Pattern;
  CompoundStmt *Action;

public:
  PatternActionDecl() : Decl(DK_PatternAction) {}
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

  Stmt(StmtKind K) : Kind(K) {}

  StmtKind GetKind() const { return Kind; }

private:
  const StmtKind Kind;
};

class CompoundStmt : public Stmt {
  std::vector<Stmt *> Stmts{};

public:
  CompoundStmt() : Stmt(SK_Compound) {}
};
