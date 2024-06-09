#pragma once

#include "Lexer/Lexer.h"
#include "Support/Sequence.h"

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
class DeclStmt;
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
  DeclKind GetKind() const { return Kind; }
};

class TranslationUnitDecl : public Decl {
  Sequence<Decl *> Decls;

protected:
  TranslationUnitDecl() : Decl(DK_TranslationUnit) {}
  TranslationUnitDecl(Sequence<Decl *> D)
      : Decl(DK_TranslationUnit), Decls(D) {}

public:
  static TranslationUnitDecl *Create(Sequence<Decl *> Decls) {
    return new TranslationUnitDecl(Decls);
  }
  static TranslationUnitDecl *CreateEmpty() { return new TranslationUnitDecl; }

  const Sequence<Decl *> &GetDecls() const { return Decls; }
};

class FunctionDecl : public Decl {
  Token Identifier;
  Sequence<ParamVarDecl *> Params;
  CompoundStmt *Body;

protected:
  FunctionDecl() : Decl(DK_Function) {}

  FunctionDecl(Token Identifier, Sequence<ParamVarDecl *> Params,
               CompoundStmt *Body)
      : Decl(DK_Function), Identifier(Identifier), Params(Params), Body(Body) {}

public:
  static FunctionDecl *Create(Token Identifier, Sequence<ParamVarDecl *> Params,
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
  static VarDecl *Create(Token Identifier) {
    return new VarDecl(DK_Var, Identifier);
  }
};

class ParamVarDecl : public VarDecl {
protected:
  ParamVarDecl(Token Identifier) : VarDecl(DK_ParamVar, Identifier) {}

public:
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
    SK_Decl,
    SK_Do,
    SK_Exit,
    SK_Expr,
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
  StmtKind GetKind() const { return Kind; }
};

class CompoundStmt : public Stmt {
  Sequence<Stmt *> Body{};

protected:
  CompoundStmt() : Stmt(SK_Compound) {}

  CompoundStmt(Sequence<Stmt *> Body) : Stmt(SK_Compound), Body(Body) {}

public:
  static CompoundStmt *Create(Sequence<Stmt *> Body) {
    return new CompoundStmt(Body);
  }

  static CompoundStmt *CreateEmpty() { return new CompoundStmt; }

  Sequence<Stmt *> GetBody() const { return Body; }
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
  static ForRangeStmt *Create(DeclRefExpr *LoopVar, DeclRefExpr *Range,
                              Stmt *Body) {
    return new ForRangeStmt(LoopVar, Range, Body);
  }
};

class BreakStmt : public Stmt {
protected:
  BreakStmt() : Stmt(SK_Break) {}

public:
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
  static ExitStmt *Create(Expr *Value) { return new ExitStmt(Value); }
};

class NextStmt : public Stmt {
protected:
  NextStmt() : Stmt(SK_Next) {}

public:
  static NextStmt *Create() { return new NextStmt; }
};

class NextfileStmt : public Stmt {
protected:
  NextfileStmt() : Stmt(SK_Nextfile) {}

public:
  static NextfileStmt *Create() { return new NextfileStmt; }
};

class WhileStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  WhileStmt(Expr *Cond, Stmt *Body) : Stmt(SK_While), Cond(Cond), Body(Body) {}

public:
  static WhileStmt *Create(Expr *Cond, Stmt *Body) {
    return new WhileStmt(Cond, Body);
  }
};

class DoStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  DoStmt(Expr *Cond, Stmt *Body) : Stmt(SK_Do), Cond(Cond), Body(Body) {}

public:
  static DoStmt *Create(Expr *Cond, Stmt *Body) {
    return new DoStmt(Cond, Body);
  }
};

class PrintStmt : public Stmt {
public:
  enum PrintKind { PK_Print, PK_Printf };

private:
  PrintKind PKind;
  Sequence<Expr *> Args;
  Expr *Output;

protected:
  PrintStmt(PrintKind PKind, Sequence<Expr *> Args, Expr *Output)
      : Stmt(SK_Print), PKind(PKind), Args(Args), Output(Output) {}

public:
  static PrintStmt *Create(PrintKind PKind, Sequence<Expr *> Args,
                           Expr *Output) {
    return new PrintStmt(PKind, Args, Output);
  }
};

class ReturnStmt : public Stmt {
  Expr *Value;

protected:
  ReturnStmt(Expr *Value) : Stmt(SK_Return), Value(Value) {}

public:
  static ReturnStmt *Create(Expr *Value) { return new ReturnStmt(Value); }
};

class ValueStmt : public Stmt {
  Expr *Value;

protected:
  ValueStmt(Expr *Value) : Stmt(SK_Value), Value(Value) {}

public:
  static ValueStmt *Create(Expr *Value) { return new ValueStmt(Value); }
};

class Expr {
public:
  enum ExprKind {
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
};

class BinaryOperator : public Expr {
  Expr *LHS;
  Expr *RHS;
  Token Opcode;

protected:
  BinaryOperator(Expr *LHS, Expr *RHS, Token Opcode)
      : Expr(EK_BinaryOperator), LHS(LHS), RHS(RHS), Opcode(Opcode) {}

public:
  static BinaryOperator *Create(Expr *LHS, Expr *RHS, Token Opcode) {
    return new BinaryOperator(LHS, RHS, Opcode);
  }
};

class CallExpr : public Expr {
  Expr *Callee;
  Sequence<Expr *> Args;

protected:
  CallExpr(Expr *Callee, Sequence<Expr *> Args)
      : Expr(EK_Call), Callee(Callee), Args(Args) {}

public:
  static CallExpr *Create(Expr *Callee, Sequence<Expr *> Args) {
    return new CallExpr(Callee, Args);
  }
};

class DeclRefExpr : public Expr {
  Token Identifier;

protected:
  DeclRefExpr(Token Identifier) : Expr(EK_DeclRef), Identifier(Identifier) {}

public:
  static DeclRefExpr *Create(Token Identifier) {
    return new DeclRefExpr(Identifier);
  }
};

class FloatingLiteral : public Expr {
  Token Value;

protected:
  FloatingLiteral(Token Value) : Expr(EK_FloatingLiteral), Value(Value) {}

public:
  static FloatingLiteral *Create(Token Value) {
    return new FloatingLiteral(Value);
  }
};

class RegexLiteral : public Expr {
  Token Value;

protected:
  RegexLiteral(Token Value) : Expr(EK_RegexLiteral), Value(Value) {}

public:
  static RegexLiteral *Create(Token Value) { return new RegexLiteral(Value); }
};

class StringLiteral : public Expr {
  Token Value;

protected:
  StringLiteral(Token Value) : Expr(EK_StringLiteral), Value(Value) {}

public:
  static StringLiteral *Create(Token Value) { return new StringLiteral(Value); }
};

class UnaryOperator : public Expr {
  Token Opcode;
  Expr *SubExpr;

protected:
  UnaryOperator(Token Opcode, Expr *SubExpr)
      : Expr(EK_UnaryOperator), Opcode(Opcode), SubExpr(SubExpr) {}

public:
  static UnaryOperator *Create(Token Opcode, Expr *SubExpr) {
    return new UnaryOperator(Opcode, SubExpr);
  }
};

} // namespace cawk

#if 0
class Expr;
class BinaryExpr;
class CallExpr;
class DeclRefExpr;
class FloatingLiteral;
class StringLiteral;
class UnaryExpr;
#endif
