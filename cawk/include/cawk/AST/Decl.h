#pragma once

#include "cawk/Basic/SourceLocation.h"
#include "cawk/Lexer/Token.h"

#include <vector>

namespace cawk {

class Expr;
class CompoundStmt;
class DeclRefExpr;
class VarDecl;

class Decl {
public:
  enum DeclKind { DK_TranslationUnit, DK_Rule, DK_Function, DK_Var };

private:
  const DeclKind Kind;
  SourceRange SrcRange;

protected:
  Decl(DeclKind K) : Kind(K) {}
  Decl(DeclKind K, SourceRange SrcRange) : Kind(K), SrcRange(SrcRange) {}

public:
  DeclKind getKind() const { return Kind; }
  SourceRange getSourceRange() const { return SrcRange; }
};

class TranslationUnitDecl : public Decl {
  std::vector<Decl *> Decls;

protected:
  TranslationUnitDecl() : Decl(DK_TranslationUnit) {}
  TranslationUnitDecl(std::vector<Decl *> D)
      : Decl(DK_TranslationUnit), Decls(D) {}

public:
  static bool classof(const Decl *D) {
    return D->getKind() == DK_TranslationUnit;
  }

  static TranslationUnitDecl *Create(std::vector<Decl *> Decls) {
    return new TranslationUnitDecl(Decls);
  }
  static TranslationUnitDecl *CreateEmpty() { return new TranslationUnitDecl; }

  const std::vector<Decl *> &getDecls() const { return Decls; }
};

class FunctionDecl : public Decl {
  Token Identifier;
  std::vector<VarDecl *> Params;
  CompoundStmt *Body;

protected:
  FunctionDecl() : Decl(DK_Function) {}

  FunctionDecl(Token Identifier, std::vector<VarDecl *> Params,
               CompoundStmt *Body, SourceRange SrcRange)
      : Decl(DK_Function, SrcRange), Identifier(Identifier), Params(Params),
        Body(Body) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_Function; }

  Token getIdentifier() const { return Identifier; }

  std::vector<VarDecl *> getParams() const { return Params; }

  CompoundStmt *getBody() { return Body; }

  const CompoundStmt *getBody() const { return Body; }

  std::string_view getName() const { return getIdentifier().getIdentifier(); }

  static FunctionDecl *Create(Token Identifier, std::vector<VarDecl *> Params,
                              CompoundStmt *Body, SourceRange SrcRange) {
    return new FunctionDecl(Identifier, Params, Body, SrcRange);
  }

  static FunctionDecl *CreateEmpty() { return new FunctionDecl; }
};

class RuleDecl : public Decl {
  Expr *Pattern;
  CompoundStmt *Action;

protected:
  RuleDecl() : Decl(DK_Rule) {}

  RuleDecl(Expr *Pattern, CompoundStmt *Action, SourceRange SrcRange)
      : Decl(DK_Rule, SrcRange), Pattern(Pattern), Action(Action) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_Rule; }

  Expr *getPattern() const { return Pattern; }

  CompoundStmt *getAction() const { return Action; }

  void setPattern(Expr *E) { Pattern = E; }

  static RuleDecl *Create(Expr *Pattern, CompoundStmt *Action,
                          SourceRange SrcRange) {
    return new RuleDecl(Pattern, Action, SrcRange);
  }
};

class VarDecl : public Decl {
protected:
  DeclRefExpr *D;

  VarDecl(DeclRefExpr *D, SourceRange SrcRange)
      : Decl(DK_Var, SrcRange), D(D) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_Var; }

  DeclRefExpr *getDeclRefExpr() { return D; }

  void setDeclRefExpr(DeclRefExpr *D) { this->D = D; }

  std::string_view getName();

  static VarDecl *Create(DeclRefExpr *D, SourceRange SrcRange) {
    return new VarDecl(D, SrcRange);
  }
};

} // namespace cawk