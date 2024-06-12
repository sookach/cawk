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
  DeclKind GetKind() const { return Kind; }
};

class TranslationUnitDecl : public Decl {
  Sequence<Decl *> Decls;

protected:
  TranslationUnitDecl() : Decl(DK_TranslationUnit) {}
  TranslationUnitDecl(Sequence<Decl *> D)
      : Decl(DK_TranslationUnit), Decls(D) {}

public:
  static bool classof(const Decl *D) {
    return D->GetKind() == DK_TranslationUnit;
  }

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
  static bool classof(const Decl *D) { return D->GetKind() == DK_Function; }

  Token GetIdentifier() { return Identifier; }

  Sequence<ParamVarDecl *> GetParams() { return Params; }

  CompoundStmt *GetBody() { return Body; }

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
  static bool classof(const Decl *D) { return D->GetKind() == DK_Rule; }

  Expr *GetPattern() const { return Pattern; }

  CompoundStmt *GetAction() const { return Action; }

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
    switch (D->GetKind()) {
    default:
      return false;
    case DK_Var:
    case DK_ParamVar:
      return true;
    };
  }

  Token GetIdentifier() { return Identifier; }

  static VarDecl *Create(Token Identifier) {
    return new VarDecl(DK_Var, Identifier);
  }
};

class ParamVarDecl : public VarDecl {
protected:
  ParamVarDecl(Token Identifier) : VarDecl(DK_ParamVar, Identifier) {}

public:
  static bool classof(const Decl *D) { return D->GetKind() == DK_ParamVar; }

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
  StmtKind GetKind() const { return Kind; }
};

class CompoundStmt : public Stmt {
  Sequence<Stmt *> Body{};

protected:
  CompoundStmt() : Stmt(SK_Compound) {}

  CompoundStmt(Sequence<Stmt *> Body) : Stmt(SK_Compound), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Compound; }

  Sequence<Stmt *> GetBody() { return Body; }

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
  static bool classof(const Stmt *S) { return S->GetKind() == SK_If; }

  Expr *GetCond() { return Cond; }

  Stmt *GetThen() { return Then; }

  Stmt *GetElse() { return Else; }

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
  static bool classof(const Stmt *S) { return S->GetKind() == SK_For; }

  Stmt *GetInit() { return Init; }

  Expr *GetCond() { return Cond; }

  Stmt *GetInc() { return Inc; }

  Stmt *GetBody() { return Body; }

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
  static bool classof(const Stmt *S) { return S->GetKind() == SK_ForRange; }

  DeclRefExpr *GetLoopVar() { return LoopVar; }

  DeclRefExpr *GetRange() { return Range; }

  Stmt *GetBody() { return Body; }

  static ForRangeStmt *Create(DeclRefExpr *LoopVar, DeclRefExpr *Range,
                              Stmt *Body) {
    return new ForRangeStmt(LoopVar, Range, Body);
  }
};

class BreakStmt : public Stmt {
protected:
  BreakStmt() : Stmt(SK_Break) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Break; }

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
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Exit; }

  Expr *GetValue() { return Value; }

  static ExitStmt *Create(Expr *Value) { return new ExitStmt(Value); }
};

class NextStmt : public Stmt {
protected:
  NextStmt() : Stmt(SK_Next) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Next; }

  static NextStmt *Create() { return new NextStmt; }
};

class NextfileStmt : public Stmt {
protected:
  NextfileStmt() : Stmt(SK_Nextfile) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Nextfile; }

  static NextfileStmt *Create() { return new NextfileStmt; }
};

class WhileStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  WhileStmt(Expr *Cond, Stmt *Body) : Stmt(SK_While), Cond(Cond), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_While; }

  Expr *GetCond() { return Cond; }

  Stmt *GetBody() { return Body; }

  static WhileStmt *Create(Expr *Cond, Stmt *Body) {
    return new WhileStmt(Cond, Body);
  }
};

class DeleteStmt : public Stmt {
  Expr *Argument;

protected:
  DeleteStmt(Expr *Argument) : Stmt(SK_Delete), Argument(Argument) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Delete; }

  Expr *GetArgument() { return Argument; }

  static DeleteStmt *Create(Expr *Argument) { return new DeleteStmt(Argument); }
};

class DoStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  DoStmt(Expr *Cond, Stmt *Body) : Stmt(SK_Do), Cond(Cond), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Do; }

  Expr *GetCond() { return Cond; }

  Stmt *GetBody() { return Body; }

  static DoStmt *Create(Expr *Cond, Stmt *Body) {
    return new DoStmt(Cond, Body);
  }
};

class PrintStmt : public Stmt {
public:
  enum PrintKind { PK_Print, PK_Printf };

private:
  Token Iden;
  Sequence<Expr *> Args;
  Token Opcode;
  Expr *Output;

protected:
  PrintStmt(Token Iden, Sequence<Expr *> Args, Token Opcode = {},
            Expr *Output = nullptr)
      : Stmt(SK_Print), Iden(Iden), Args(Args), Opcode(Opcode), Output(Output) {
  }

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Print; }

  Token GetIden() { return Iden; }

  Sequence<Expr *> GetArgs() { return Args; }

  Token GetOpcode() { return Opcode; }

  Expr *GetOutput() { return Output; }

  static PrintStmt *Create(Token Iden, Sequence<Expr *> Args, Token Opcode = {},
                           Expr *Output = nullptr) {
    return new PrintStmt(Iden, Args, Opcode, Output);
  }
};

class ReturnStmt : public Stmt {
  Expr *Value;

protected:
  ReturnStmt(Expr *Value) : Stmt(SK_Return), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Return; }

  Expr *GetValue() { return Value; }

  static ReturnStmt *Create(Expr *Value) { return new ReturnStmt(Value); }
};

class ValueStmt : public Stmt {
  Expr *Value;

protected:
  ValueStmt(Expr *Value) : Stmt(SK_Value), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->GetKind() == SK_Value; }

  Expr *GetValue() { return Value; }

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
  ExprKind GetKind() const { return Kind; }
};

class ArraySubscriptExpr : public Expr {
  Expr *LHS;
  Expr *RHS;

protected:
  ArraySubscriptExpr(Expr *LHS, Expr *RHS)
      : Expr(EK_ArraySubscript), LHS(LHS), RHS(RHS) {}

public:
  static bool classof(const Expr *E) {
    return E->GetKind() == EK_ArraySubscript;
  }

  Expr *GetLHS() const { return LHS; }

  void SetLHS(Expr *E) { LHS = E; }

  Expr *GetRHS() const { return RHS; }

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
    return E->GetKind() == EK_BinaryOperator;
  }

  Expr *GetLHS() { return LHS; }

  Expr *GetRHS() { return RHS; }

  Token GetOpcode() { return Opcode; }

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
  static bool classof(const Expr *E) { return E->GetKind() == EK_Call; }

  Expr *GetCallee() { return Callee; }

  Sequence<Expr *> GetArgs() { return Args; }

  static CallExpr *Create(Expr *Callee, Sequence<Expr *> Args) {
    return new CallExpr(Callee, Args);
  }
};

class DeclRefExpr : public Expr {
  Token Identifier;

protected:
  DeclRefExpr(Token Identifier) : Expr(EK_DeclRef), Identifier(Identifier) {}

public:
  static bool classof(const Expr *E) { return E->GetKind() == EK_DeclRef; }

  Token GetIdentifier() { return Identifier; }

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
    return E->GetKind() == EK_FloatingLiteral;
  }

  Token GetValue() { return Value; }

  static FloatingLiteral *Create(Token Value) {
    return new FloatingLiteral(Value);
  }
};

class RegexLiteral : public Expr {
  Token Value;

protected:
  RegexLiteral(Token Value) : Expr(EK_RegexLiteral), Value(Value) {}

public:
  static bool classof(const Expr *E) { return E->GetKind() == EK_RegexLiteral; }

  Token GetValue() { return Value; }

  static RegexLiteral *Create(Token Value) { return new RegexLiteral(Value); }
};

class StringLiteral : public Expr {
  Token Value;

protected:
  StringLiteral(Token Value) : Expr(EK_StringLiteral), Value(Value) {}

public:
  static bool classof(const Expr *E) {
    return E->GetKind() == EK_StringLiteral;
  }

  Token GetValue() { return Value; }

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
  Token GetOpcode() { return Opcode; }

  Expr *GetSubExpr() { return SubExpr; }

  static UnaryOperator *Create(Token Opcode, Expr *SubExpr, FixKind Fix) {
    return new UnaryOperator(Opcode, SubExpr, Fix);
  }
};

} // namespace cawk
