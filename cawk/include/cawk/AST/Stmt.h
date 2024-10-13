#pragma once

#include "cawk/Basic/SourceLocation.h"

#include <vector>

namespace cawk {
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
    SK_Null,
    SK_Print,
    SK_Return,
    SK_Value,
    SK_While
  };

private:
  const StmtKind Kind;
  SourceRange SrcRange;

protected:
  Stmt(StmtKind Kind) : Kind(Kind) {}
  Stmt(StmtKind Kind, SourceRange SrcRange) : Kind(Kind), SrcRange(SrcRange) {}

public:
  StmtKind getKind() const { return Kind; }
  SourceRange getSourceRange() const { return SrcRange; }
};

class CompoundStmt : public Stmt {
  std::vector<Stmt *> Body{};

protected:
  CompoundStmt() : Stmt(SK_Compound) {}

  CompoundStmt(std::vector<Stmt *> Body, SourceRange SrcRange)
      : Stmt(SK_Compound, SrcRange), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Compound; }

  std::vector<Stmt *> getBody() { return Body; }

  void setBody(std::vector<Stmt *> B) { Body = B; }

  static CompoundStmt *Create(std::vector<Stmt *> Body, SourceRange SrcRange) {
    return new CompoundStmt(Body, SrcRange);
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

  IfStmt(Expr *Cond, Stmt *Then, Stmt *Else, SourceRange SrcRange)
      : Stmt(SK_If, SrcRange), Cond(Cond), Then(Then), Else(Else) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_If; }

  Expr *getCond() { return Cond; }

  Stmt *getThen() { return Then; }

  Stmt *getElse() { return Else; }

  void setCond(Expr *E) { Cond = E; }

  void setThen(Stmt *S) { Then = S; }

  void setElse(Stmt *S) { Else = S; }

  static IfStmt *Create(Expr *Cond, Stmt *Then, Stmt *Else,
                        SourceRange SrcRange) {
    return new IfStmt(Cond, Then, Else, SrcRange);
  }
};

class ForStmt : public Stmt {
  Stmt *Init;
  Expr *Cond;
  Stmt *Inc;
  Stmt *Body;

protected:
  ForStmt() : Stmt(SK_For) {}

  ForStmt(Stmt *Init, Expr *Cond, Stmt *Inc, Stmt *Body, SourceRange SrcRange)
      : Stmt(SK_For, SrcRange), Init(Init), Cond(Cond), Inc(Inc), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_For; }

  Stmt *getInit() { return Init; }

  Expr *getCond() { return Cond; }

  Stmt *getInc() { return Inc; }

  Stmt *getBody() { return Body; }

  void setCond(Expr *E) { Cond = E; }

  void setBody(Stmt *S) { Body = S; }

  static ForStmt *Create(Stmt *Init, Expr *Cond, Stmt *Inc, Stmt *Body,
                         SourceRange SrcRange) {
    return new ForStmt(Init, Cond, Inc, Body, SrcRange);
  }
};

class ForRangeStmt : public Stmt {
  DeclRefExpr *LoopVar;
  DeclRefExpr *Range;
  Stmt *Body;

protected:
  ForRangeStmt(DeclRefExpr *LoopVar, DeclRefExpr *Range, Stmt *Body,
               SourceRange SrcRange)
      : Stmt(SK_ForRange, SrcRange), LoopVar(LoopVar), Range(Range),
        Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_ForRange; }

  DeclRefExpr *getLoopVar() { return LoopVar; }

  DeclRefExpr *getRange() { return Range; }

  void setLoopVar(DeclRefExpr *E) { LoopVar = E; }

  void setRange(DeclRefExpr *E) { Range = E; }

  Stmt *getBody() { return Body; }

  void setBody(Stmt *S) { Body = S; }

  static ForRangeStmt *Create(DeclRefExpr *LoopVar, DeclRefExpr *Range,
                              Stmt *Body, SourceRange SrcRange) {
    return new ForRangeStmt(LoopVar, Range, Body, SrcRange);
  }
};

class BreakStmt : public Stmt {
protected:
  BreakStmt(SourceRange SrcRange) : Stmt(SK_Break, SrcRange) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Break; }

  static BreakStmt *Create(SourceRange SrcRange) {
    return new BreakStmt(SrcRange);
  }
};

class ContinueStmt : public Stmt {
protected:
  ContinueStmt(SourceRange SrcRange) : Stmt(SK_Continue, SrcRange) {}

public:
  static ContinueStmt *Create(SourceRange SrcRange) {
    return new ContinueStmt(SrcRange);
  }
};

class ExitStmt : public Stmt {
  Expr *Value;

protected:
  ExitStmt(Expr *Value, SourceRange SrcRange)
      : Stmt(SK_Exit, SrcRange), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Exit; }

  Expr *getValue() { return Value; }

  void setValue(Expr *E) { Value = E; }

  static ExitStmt *Create(Expr *Value, SourceRange SrcRange) {
    return new ExitStmt(Value, SrcRange);
  }
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
  NextfileStmt(SourceRange SrcRange) : Stmt(SK_Nextfile, SrcRange) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Nextfile; }

  static NextfileStmt *Create(SourceRange SrcRange) {
    return new NextfileStmt(SrcRange);
  }
};

class WhileStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  WhileStmt(Expr *Cond, Stmt *Body, SourceRange SrcRange)
      : Stmt(SK_While, SrcRange), Cond(Cond), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_While; }

  Expr *getCond() { return Cond; }

  Stmt *getBody() { return Body; }

  void setCond(Expr *E) { Cond = E; }

  void setBody(Stmt *S) { Body = S; }

  static WhileStmt *Create(Expr *Cond, Stmt *Body, SourceRange SrcRange) {
    return new WhileStmt(Cond, Body, SrcRange);
  }
};

class DeleteStmt : public Stmt {
  Expr *Argument;

protected:
  DeleteStmt(Expr *Argument, SourceRange SrcRange)
      : Stmt(SK_Delete, SrcRange), Argument(Argument) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Delete; }

  Expr *getArgument() { return Argument; }

  void setArgument(Expr *E) { Argument = E; }

  static DeleteStmt *Create(Expr *Argument, SourceRange SrcRange) {
    return new DeleteStmt(Argument, SrcRange);
  }
};

class DoStmt : public Stmt {
  Expr *Cond;
  Stmt *Body;

protected:
  DoStmt(Expr *Cond, Stmt *Body, SourceRange SrcRange)
      : Stmt(SK_Do, SrcRange), Cond(Cond), Body(Body) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Do; }

  Expr *getCond() { return Cond; }

  Stmt *getBody() { return Body; }

  void setBody(Stmt *S) { Body = S; }

  static DoStmt *Create(Expr *Cond, Stmt *Body, SourceRange SrcRange) {
    return new DoStmt(Cond, Body, SrcRange);
  }
};

class NullStmt : public Stmt {

protected:
  NullStmt(SourceRange SrcRange) : Stmt(SK_Null, SrcRange) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Null; }

  static NullStmt *Create(SourceRange SrcRange) {
    return new NullStmt(SrcRange);
  }
};

class PrintStmt : public Stmt {
  Token Iden;
  std::vector<Expr *> Args;
  Token Opcode;
  Expr *Output;

protected:
  PrintStmt(Token Iden, std::vector<Expr *> Args, Token Opcode, Expr *Output,
            SourceRange SrcRange)
      : Stmt(SK_Print, SrcRange), Iden(Iden), Args(Args), Opcode(Opcode),
        Output(Output) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Print; }

  Token getIden() { return Iden; }

  std::vector<Expr *> getArgs() { return Args; }

  Token getOpcode() { return Opcode; }

  Expr *getOutput() { return Output; }

  void setArg(std::size_t I, Expr *E) {
    assert(I < std::size(Args));
    Args[I] = E;
  }

  static PrintStmt *Create(Token Iden, std::vector<Expr *> Args, Token Opcode,
                           Expr *Output, SourceRange SrcRange) {
    return new PrintStmt(Iden, Args, Opcode, Output, SrcRange);
  }
};

class ReturnStmt : public Stmt {
  Expr *Value;

protected:
  ReturnStmt(Expr *Value, SourceRange SrcRange)
      : Stmt(SK_Return, SrcRange), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Return; }

  Expr *getValue() { return Value; }

  void setValue(Expr *E) { Value = E; }

  static ReturnStmt *Create(Expr *Value, SourceRange SrcRange) {
    return new ReturnStmt(Value, SrcRange);
  }
};

class ValueStmt : public Stmt {
  Expr *Value;

protected:
  ValueStmt(Expr *Value, SourceRange SrcRange)
      : Stmt(SK_Value, SrcRange), Value(Value) {}

public:
  static bool classof(const Stmt *S) { return S->getKind() == SK_Value; }

  Expr *getValue() { return Value; }

  void setValue(Expr *E) { Value = E; }

  static ValueStmt *Create(Expr *Value, SourceRange SrcRange) {
    return new ValueStmt(Value, SrcRange);
  }
};
} // namespace cawk