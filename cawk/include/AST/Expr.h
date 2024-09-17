#pragma once

#include "Basic/SourceLocation.h"

#include <vector>

namespace cawk {
class Expr {
public:
  enum ExprKind {
    EK_ArraySubscript,
    EK_Begin,
    EK_BinaryOperator,
    EK_Call,
    EK_DeclRef,
    EK_End,
    EK_FloatingLiteral,
    EK_Lambda,
    EK_RegexLiteral,
    EK_StringLiteral,
    EK_UnaryOperator
  };

protected:
  Expr(ExprKind Kind) : Kind(Kind) {}
  Expr(ExprKind Kind, SourceRange SrcRange) : Kind(Kind), SrcRange(SrcRange) {}

private:
  const ExprKind Kind;
  SourceRange SrcRange;
  bool IsLValue = false;

public:
  ExprKind getKind() const { return Kind; }

  SourceRange getSourceRange() const { return SrcRange; }

  bool isLValue() const { return IsLValue; }

  void markAsLValue() { IsLValue = true; }
};

class ArraySubscriptExpr : public Expr {
  Expr *LHS;
  std::vector<Expr *> RHS;

protected:
  ArraySubscriptExpr(Expr *LHS, std::vector<Expr *> RHS, SourceRange SrcRange)
      : Expr(EK_ArraySubscript, SrcRange), LHS(LHS), RHS(RHS) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_ArraySubscript;
  }

  Expr *getLHS() const { return LHS; }

  void setLHS(Expr *E) { LHS = E; }

  std::vector<Expr *> getRHS() const { return RHS; }

  void setRHS(int I, Expr *E) {
    assert(I < std::size(RHS));
    RHS[I] = E;
  }

  void setRHS(std::vector<Expr *> E) { RHS = E; }

  static ArraySubscriptExpr *Create(Expr *LHS, std::vector<Expr *> RHS,
                                    SourceRange SrcRange) {
    return new ArraySubscriptExpr(LHS, RHS, SrcRange);
  }
};

class BeginKeyword : public Expr {
protected:
  BeginKeyword(SourceRange SrcRange) : Expr(EK_Begin, SrcRange) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_Begin; }

  static BeginKeyword *Create(SourceRange SrcRange) {
    return new BeginKeyword(SrcRange);
  }
};

class BinaryOperator : public Expr {
  Expr *LHS;
  Expr *RHS;
  Token Opcode;

protected:
  BinaryOperator(Expr *LHS, Expr *RHS, Token Opcode, SourceRange SrcRange)
      : Expr(EK_BinaryOperator, SrcRange), LHS(LHS), RHS(RHS), Opcode(Opcode) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_BinaryOperator;
  }

  Expr *getLHS() { return LHS; }

  void setLHS(Expr *E) { LHS = E; }

  Expr *getRHS() { return RHS; }

  void setRHS(Expr *E) { RHS = E; }

  Token getOpcode() { return Opcode; }

  static BinaryOperator *Create(Expr *LHS, Expr *RHS, Token Opcode,
                                SourceRange SrcRange) {
    return new BinaryOperator(LHS, RHS, Opcode, SrcRange);
  }
};

class CallExpr : public Expr {
  Expr *Callee;
  std::vector<Expr *> Args;

protected:
  CallExpr(Expr *Callee, std::vector<Expr *> Args, SourceRange SrcRange)
      : Expr(EK_Call, SrcRange), Callee(Callee), Args(Args) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_Call; }

  Expr *getCallee() { return Callee; }

  std::vector<Expr *> getArgs() { return Args; }

  void setArg(int I, Expr *E) {
    assert(I < std::size(Args));
    Args[I] = E;
  }

  static CallExpr *Create(Expr *Callee, std::vector<Expr *> Args,
                          SourceRange SrcRange) {
    return new CallExpr(Callee, Args, SrcRange);
  }
};

class DeclRefExpr : public Expr {
  Token Identifier;

protected:
  DeclRefExpr(Token Identifier, SourceRange SrcRange)
      : Expr(EK_DeclRef, SrcRange), Identifier(Identifier) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_DeclRef; }

  Token getIdentifier() const { return Identifier; }

  std::string_view getName() const { return getIdentifier().getRawData(); }

  static DeclRefExpr *Create(Token Identifier, SourceRange SrcRange) {
    return new DeclRefExpr(Identifier, SrcRange);
  }
};

class EndKeyword : public Expr {
protected:
  EndKeyword(SourceRange SrcRange) : Expr(EK_End, SrcRange) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_Begin; }

  static EndKeyword *Create(SourceRange SrcRange) {
    return new EndKeyword(SrcRange);
  }
};

class FloatingLiteral : public Expr {
  Token Literal;

protected:
  FloatingLiteral(Token Literal, SourceRange SrcRange)
      : Expr(EK_FloatingLiteral), Literal(Literal) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_FloatingLiteral;
  }

  Token getLiteral() { return Literal; }

  std::string_view getLiteralData() { return Literal.getLiteralData(); }

  static FloatingLiteral *Create(Token Literal, SourceRange SrcRange) {
    return new FloatingLiteral(Literal, SrcRange);
  }
};

class LambdaExpr : public Expr {
  std::vector<VarDecl *> Params;
  CompoundStmt *Body;

protected:
  LambdaExpr(std::vector<VarDecl *> Params, CompoundStmt *Body,
             SourceRange SrcRange)
      : Expr(EK_Lambda, SrcRange), Params(Params), Body(Body) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_Lambda; }

  std::vector<VarDecl *> getParams() { return Params; }

  CompoundStmt *getBody() { return Body; }

  static LambdaExpr *Create(std::vector<VarDecl *> Params, CompoundStmt *Body,
                            SourceRange SrcRange) {
    return new LambdaExpr(Params, Body, SrcRange);
  }
};

class RegexLiteral : public Expr {
  Token Literal;

protected:
  RegexLiteral(Token Literal, SourceRange SrcRange)
      : Expr(EK_RegexLiteral), Literal(Literal) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_RegexLiteral; }

  Token getLiteral() { return Literal; }

  static RegexLiteral *Create(Token Literal, SourceRange SrcRange) {
    return new RegexLiteral(Literal, SrcRange);
  }
};

class StringLiteral : public Expr {
  Token Literal;

protected:
  StringLiteral(Token Literal, SourceRange SrcRange)
      : Expr(EK_StringLiteral, SrcRange), Literal(Literal) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_StringLiteral;
  }

  Token getLiteral() { return Literal; }

  std::string_view getLiteralData() { return Literal.getLiteralData(); }

  static StringLiteral *Create(Token Value, SourceRange SrcRange) {
    return new StringLiteral(Value, SrcRange);
  }
};

class UnaryOperator : public Expr {
public:
  enum FixKind { Prefix, Postfix };

private:
  Token Opcode;
  Expr *SubExpr;
  FixKind Fix;

protected:
  UnaryOperator(Token Opcode, Expr *SubExpr, FixKind Fix, SourceRange SrcRange)
      : Expr(EK_UnaryOperator, SrcRange), Opcode(Opcode), SubExpr(SubExpr),
        Fix(Fix) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_UnaryOperator;
  }

  Token getOpcode() { return Opcode; }

  Expr *getSubExpr() { return SubExpr; }

  void setSubExpr(Expr *E) { SubExpr = E; }

  FixKind getFix() { return Fix; }

  static UnaryOperator *Create(Token Opcode, Expr *SubExpr, FixKind Fix,
                               SourceRange SrcRange) {
    return new UnaryOperator(Opcode, SubExpr, Fix, SrcRange);
  }
};
} // namespace cawk