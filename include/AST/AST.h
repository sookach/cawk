#pragma once

#include "Lexer/Lexer.h"

#include <vector>

namespace cawk {

class Decl;
class TranslationUnitDecl;
class RuleDecl;
class FunctionDecl;
class VarDecl;
class ParamVarDecl;

class Stmt;
class BreakStmt;
class ContinueStmt;
class CompoundStmt;
class DeleteStmt;
class DoStmt;
class ExitStmt;
class ForStmt;
class ForRangeStmt;
class IfStmt;
class NextStmt;
class NextfileStmt;
class PrintStmt;
class ReturnStmt;
class ValueStmt;
class WhileStmt;

class Expr;
class BinaryOperator;
class CallExpr;
class DeclRefExpr;
class FloatingLiteral;
class RegexLiteral;
class StringLiteral;
class UnaryOperator;

class Decl {
public:
  enum DeclKind {
    DK_TranslationUnit,
    DK_Rule,
    DK_Function,
    DK_Var,
    DK_ParamVar
  };

private:
  const DeclKind Kind;

protected:
  Decl(DeclKind K) : Kind(K) {}

public:
  DeclKind getKind() const { return Kind; }
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
  std::vector<ParamVarDecl *> Params;
  CompoundStmt *Body;

protected:
  FunctionDecl() : Decl(DK_Function) {}

  FunctionDecl(Token Identifier, std::vector<ParamVarDecl *> Params,
               CompoundStmt *Body)
      : Decl(DK_Function), Identifier(Identifier), Params(Params), Body(Body) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_Function; }

  Token getIdentifier() const { return Identifier; }

  std::vector<ParamVarDecl *> getParams() const { return Params; }

  CompoundStmt *getBody() { return Body; }

  const CompoundStmt *getBody() const { return Body; }

  static FunctionDecl *Create(Token Identifier,
                              std::vector<ParamVarDecl *> Params,
                              CompoundStmt *Body) {
    return new FunctionDecl(Identifier, Params, Body);
  }

  static FunctionDecl *CreateEmpty() { return new FunctionDecl; }
};

class RuleDecl : public Decl {
  Expr *Pattern;
  CompoundStmt *Action;

protected:
  RuleDecl() : Decl(DK_Rule) {}

  RuleDecl(Expr *Pattern, CompoundStmt *Action)
      : Decl(DK_Rule), Pattern(Pattern), Action(Action) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_Rule; }

  Expr *getPattern() const { return Pattern; }

  CompoundStmt *getAction() const { return Action; }

  static RuleDecl *Create(Expr *Pattern, CompoundStmt *Action) {
    return new RuleDecl(Pattern, Action);
  }
};

class VarDecl : public Decl {
protected:
  Token Identifier;

  VarDecl(DeclKind Kind, Token Identifier)
      : Decl(Kind), Identifier(Identifier) {}

public:
  static bool classof(const Decl *D) {
    switch (D->getKind()) {
    default:
      return false;
    case DK_Var:
    case DK_ParamVar:
      return true;
    };
  }

  Token getIdentifier() { return Identifier; }

  static VarDecl *Create(Token Identifier) {
    return new VarDecl(DK_Var, Identifier);
  }
};

class ParamVarDecl : public VarDecl {
protected:
  ParamVarDecl(Token Identifier) : VarDecl(DK_ParamVar, Identifier) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_ParamVar; }

  static ParamVarDecl *Create(Token Identifier) {
    return new ParamVarDecl(Identifier);
  }
};

class Stmt {
public:
  enum StmtKind {
    SK_Break,
    SK_Compound,
    SK_Continue,
    SK_Delete,
    SK_Do,
    SK_Exit,
    SK_For,
    SK_ForRange,
    SK_If,
    SK_Next,
    SK_Nextfile,
    SK_Print,
    SK_Return,
    SK_Value,
    SK_While
  };

private:
  const StmtKind Kind;

protected:
  Stmt(StmtKind Kind) : Kind(Kind) {}

public:
  StmtKind getKind() const { return Kind; }
};

class CompoundStmt : public Stmt {
  std::vector<Stmt *> Body{};

protected:
  CompoundStmt() : Stmt(SK_Compound) {}

  CompoundStmt(std::vector<Stmt *> Body) : Stmt(SK_Compound), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Compound; }

  std::vector<Stmt *> getBody() { return Body; }

  static CompoundStmt *Create(std::vector<Stmt *> Body) {
    return new CompoundStmt(Body);
  }

  static CompoundStmt *CreateEmpty() { return new CompoundStmt; }

  std::vector<Stmt *> getBody() const { return Body; }
};

class IfStmt : public Stmt {
  Expr *Cond;
  Stmt *Then;
  Stmt *Else;

protected:
  IfStmt() : Stmt(SK_If) {}

  IfStmt(Expr *Cond, Stmt *Then, Stmt *Else)
      : Stmt(SK_If), Cond(Cond), Then(Then), Else(Else) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_If; }

  Expr *getCond() { return Cond; }

  Stmt *getThen() { return Then; }

  Stmt *getElse() { return Else; }

  static IfStmt *Create(Expr *Cond, Stmt *Then, Stmt *Else) {
    return new IfStmt(Cond, Then, Else);
  }
};

class ForStmt : public Stmt {
  Stmt *Init;
  Expr *Cond;
  Stmt *Inc;
  Stmt *Body;

protected:
  ForStmt() : Stmt(SK_For) {}

  ForStmt(Stmt *Init, Expr *Cond, Stmt *Inc, Stmt *Body)
      : Stmt(SK_For), Init(Init), Cond(Cond), Inc(Inc), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_For; }

  Stmt *getInit() { return Init; }

  Expr *getCond() { return Cond; }

  Stmt *getInc() { return Inc; }

  Stmt *getBody() { return Body; }

  static ForStmt *Create(Stmt *Init, Expr *Cond, Stmt *Inc, Stmt *Body) {
    return new ForStmt(Init, Cond, Inc, Body);
  }
};

class ForRangeStmt : public Stmt {
  DeclRefExpr *LoopVar;
  DeclRefExpr *Range;
  Stmt *Body;

protected:
  ForRangeStmt(DeclRefExpr *LoopVar, DeclRefExpr *Range, Stmt *Body)
      : Stmt(SK_ForRange), LoopVar(LoopVar), Range(Range), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_ForRange; }

  DeclRefExpr *getLoopVar() { return LoopVar; }

  DeclRefExpr *getRange() { return Range; }

  Stmt *getBody() { return Body; }

  static ForRangeStmt *Create(DeclRefExpr *LoopVar, DeclRefExpr *Range,
                              Stmt *Body) {
    return new ForRangeStmt(LoopVar, Range, Body);
  }
};

class BreakStmt : public Stmt {
protected:
  BreakStmt() : Stmt(SK_Break) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Break; }

  static BreakStmt *Create() { return new BreakStmt; }
};

class ContinueStmt : public Stmt {
protected:
  ContinueStmt() : Stmt(SK_Continue) {}

public:
  static ContinueStmt *Create() { return new ContinueStmt; }
};

class ExitStmt : public Stmt {
  Expr *Value;

protected:
  ExitStmt(Expr *Value) : Stmt(SK_Exit), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Exit; }

  Expr *getValue() { return Value; }

  static ExitStmt *Create(Expr *Value) { return new ExitStmt(Value); }
};

class NextStmt : public Stmt {
protected:
  NextStmt() : Stmt(SK_Next) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Next; }

  static NextStmt *Create() { return new NextStmt; }
};

class NextfileStmt : public Stmt {
protected:
  NextfileStmt() : Stmt(SK_Nextfile) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Nextfile; }

  static NextfileStmt *Create() { return new NextfileStmt; }
};

class WhileStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  WhileStmt(Expr *Cond, Stmt *Body) : Stmt(SK_While), Cond(Cond), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_While; }

  Expr *getCond() { return Cond; }

  Stmt *getBody() { return Body; }

  static WhileStmt *Create(Expr *Cond, Stmt *Body) {
    return new WhileStmt(Cond, Body);
  }
};

class DeleteStmt : public Stmt {
  Expr *Argument;

protected:
  DeleteStmt(Expr *Argument) : Stmt(SK_Delete), Argument(Argument) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Delete; }

  Expr *getArgument() { return Argument; }

  static DeleteStmt *Create(Expr *Argument) { return new DeleteStmt(Argument); }
};

class DoStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  DoStmt(Expr *Cond, Stmt *Body) : Stmt(SK_Do), Cond(Cond), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Do; }

  Expr *getCond() { return Cond; }

  Stmt *getBody() { return Body; }

  static DoStmt *Create(Expr *Cond, Stmt *Body) {
    return new DoStmt(Cond, Body);
  }
};

class PrintStmt : public Stmt {
public:
  enum PrintKind { PK_Print, PK_Printf };

private:
  Token Iden;
  std::vector<Expr *> Args;
  Token Opcode;
  Expr *Output;

protected:
  PrintStmt(Token Iden, std::vector<Expr *> Args, Token Opcode = {},
            Expr *Output = nullptr)
      : Stmt(SK_Print), Iden(Iden), Args(Args), Opcode(Opcode), Output(Output) {
  }

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Print; }

  Token getIden() { return Iden; }

  std::vector<Expr *> getArgs() { return Args; }

  Token getOpcode() { return Opcode; }

  Expr *getOutput() { return Output; }

  static PrintStmt *Create(Token Iden, std::vector<Expr *> Args,
                           Token Opcode = {}, Expr *Output = nullptr) {
    return new PrintStmt(Iden, Args, Opcode, Output);
  }
};

class ReturnStmt : public Stmt {
  Expr *Value;

protected:
  ReturnStmt(Expr *Value) : Stmt(SK_Return), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Return; }

  Expr *getValue() { return Value; }

  static ReturnStmt *Create(Expr *Value) { return new ReturnStmt(Value); }
};

class ValueStmt : public Stmt {
  Expr *Value;

protected:
  ValueStmt(Expr *Value) : Stmt(SK_Value), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Value; }

  Expr *getValue() { return Value; }

  static ValueStmt *Create(Expr *Value) { return new ValueStmt(Value); }
};

class Expr {
public:
  enum ExprKind {
    EK_ArraySubscript,
    EK_BinaryOperator,
    EK_Call,
    EK_DeclRef,
    EK_FloatingLiteral,
    EK_RegexLiteral,
    EK_StringLiteral,
    EK_UnaryOperator
  };

protected:
  Expr(ExprKind Kind) : Kind(Kind) {}

private:
  const ExprKind Kind;

public:
  ExprKind getKind() const { return Kind; }
};

class ArraySubscriptExpr : public Expr {
  Expr *LHS;
  Expr *RHS;

protected:
  ArraySubscriptExpr(Expr *LHS, Expr *RHS)
      : Expr(EK_ArraySubscript), LHS(LHS), RHS(RHS) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_ArraySubscript;
  }

  Expr *getLHS() const { return LHS; }

  void SetLHS(Expr *E) { LHS = E; }

  Expr *getRHS() const { return RHS; }

  void SetRHS(Expr *E) { RHS = E; }

  static ArraySubscriptExpr *Create(Expr *LHS, Expr *RHS) {
    return new ArraySubscriptExpr(LHS, RHS);
  }
};

class BinaryOperator : public Expr {
  Expr *LHS;
  Expr *RHS;
  Token Opcode;

protected:
  BinaryOperator(Expr *LHS, Expr *RHS, Token Opcode)
      : Expr(EK_BinaryOperator), LHS(LHS), RHS(RHS), Opcode(Opcode) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_BinaryOperator;
  }

  Expr *getLHS() { return LHS; }

  Expr *getRHS() { return RHS; }

  Token getOpcode() { return Opcode; }

  static BinaryOperator *Create(Expr *LHS, Expr *RHS, Token Opcode) {
    return new BinaryOperator(LHS, RHS, Opcode);
  }
};

class CallExpr : public Expr {
  Expr *Callee;
  std::vector<Expr *> Args;

protected:
  CallExpr(Expr *Callee, std::vector<Expr *> Args)
      : Expr(EK_Call), Callee(Callee), Args(Args) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_Call; }

  Expr *getCallee() { return Callee; }

  std::vector<Expr *> getArgs() { return Args; }

  static CallExpr *Create(Expr *Callee, std::vector<Expr *> Args) {
    return new CallExpr(Callee, Args);
  }
};

class DeclRefExpr : public Expr {
  Token Identifier;

protected:
  DeclRefExpr(Token Identifier) : Expr(EK_DeclRef), Identifier(Identifier) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_DeclRef; }

  Token getIdentifier() const { return Identifier; }

  static DeclRefExpr *Create(Token Identifier) {
    return new DeclRefExpr(Identifier);
  }
};

class FloatingLiteral : public Expr {
  Token Value;

protected:
  FloatingLiteral(Token Value) : Expr(EK_FloatingLiteral), Value(Value) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_FloatingLiteral;
  }

  Token getValue() { return Value; }

  static FloatingLiteral *Create(Token Value) {
    return new FloatingLiteral(Value);
  }
};

class RegexLiteral : public Expr {
  Token Value;

protected:
  RegexLiteral(Token Value) : Expr(EK_RegexLiteral), Value(Value) {}

public:
  static bool classof(const Expr *E) { return E->getKind() == EK_RegexLiteral; }

  Token getValue() { return Value; }

  static RegexLiteral *Create(Token Value) { return new RegexLiteral(Value); }
};

class StringLiteral : public Expr {
  Token Value;

protected:
  StringLiteral(Token Value) : Expr(EK_StringLiteral), Value(Value) {}

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_StringLiteral;
  }

  Token getValue() { return Value; }

  static StringLiteral *Create(Token Value) { return new StringLiteral(Value); }
};

class UnaryOperator : public Expr {
public:
  enum FixKind { Prefix, Postfix };

private:
  Token Opcode;
  Expr *SubExpr;
  FixKind Fix;

protected:
  UnaryOperator(Token Opcode, Expr *SubExpr, FixKind Fix)
      : Expr(EK_UnaryOperator), Opcode(Opcode), SubExpr(SubExpr), Fix(Fix) {}

public:
  Token getOpcode() { return Opcode; }

  Expr *getSubExpr() { return SubExpr; }

  FixKind getFix() { return Fix; }

  static UnaryOperator *Create(Token Opcode, Expr *SubExpr, FixKind Fix) {
    return new UnaryOperator(Opcode, SubExpr, Fix);
  }
};

} // namespace cawk
