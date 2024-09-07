#pragma once

#include "Basic/SourceLocation.h"
#include "Exec/Value.h"
#include "Lexer/Lexer.h"

#include <functional>
#include <unordered_set>
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
class ArraySubscriptExpr;
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
  std::vector<ParamVarDecl *> Params;
  CompoundStmt *Body;
  std::unordered_set<TypeKind> ReturnTypes;

protected:
  FunctionDecl() : Decl(DK_Function) {}

  FunctionDecl(Token Identifier, std::vector<ParamVarDecl *> Params,
               CompoundStmt *Body, SourceRange SrcRange)
      : Decl(DK_Function, SrcRange), Identifier(Identifier), Params(Params),
        Body(Body) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_Function; }

  Token getIdentifier() const { return Identifier; }

  std::vector<ParamVarDecl *> getParams() const { return Params; }

  CompoundStmt *getBody() { return Body; }

  const CompoundStmt *getBody() const { return Body; }

  std::string_view getName() const { return getIdentifier().getIdentifier(); }

  void addReturnType(TypeKind T) { ReturnTypes.insert(T); }

  std::unordered_set<TypeKind> getReturnTypes() const { return ReturnTypes; }

  static FunctionDecl *Create(Token Identifier,
                              std::vector<ParamVarDecl *> Params,
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
  Token Identifier;
  Value Val;
  DeclRefExpr *E;

  VarDecl(DeclKind Kind, Token Identifier, SourceRange SrcRange)
      : Decl(Kind, SrcRange), Identifier(Identifier) {}

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

  std::string_view getName() { return getIdentifier().getIdentifier(); }

  DeclRefExpr *getExpr() { return E; }

  void setExpr(DeclRefExpr *D) { E = D; }

  static VarDecl *Create(Token Identifier, SourceRange SrcRange) {
    return new VarDecl(DK_Var, Identifier, SrcRange);
  }
};

class ParamVarDecl : public VarDecl {
protected:
  ParamVarDecl(Token Identifier, SourceRange SrcRange)
      : VarDecl(DK_ParamVar, Identifier, SrcRange) {}

public:
  static bool classof(const Decl *D) { return D->getKind() == DK_ParamVar; }

  static ParamVarDecl *Create(Token Identifier, SourceRange SrcRange) {
    return new ParamVarDecl(Identifier, SrcRange);
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

  static DoStmt *Create(Expr *Cond, Stmt *Body, SourceRange SrcRange) {
    return new DoStmt(Cond, Body, SrcRange);
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
  Value *Val = new Value;
  std::function<void()> OnAssignment = []() {};

public:
  ExprKind getKind() const { return Kind; }

  SourceRange getSourceRange() const { return SrcRange; }

  bool isLValue() const { return IsLValue; }

  void markAsLValue() { IsLValue = true; }

  Value *getValue() { return Val; }

  template <TypeKind T> auto getValueAs() { return Val->getAs<T>(); }

  void setValue(Value::Scalar S) { Val->setValue(S); }

  void setValue(Value V) { *Val = V; }

  void setValue(Value *V) { Val = V; }

  TypeKind getType() { return Val->getType(); }

  void setOnAssignment(std::function<void()> F) { OnAssignment = F; }

  void executeOnAssignment() { OnAssignment(); }
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
    static Value *TheValue = new Value;
    BeginKeyword *Raw = new BeginKeyword(SrcRange);
    Raw->setValue(TheValue);
    return Raw;
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
    static Value *TheValue = new Value;
    EndKeyword *Raw = new EndKeyword(SrcRange);
    Raw->setValue(TheValue);
    return Raw;
  }
};

class FloatingLiteral : public Expr {
  Token Literal;

protected:
  FloatingLiteral(Token Literal, SourceRange SrcRange)
      : Expr(EK_FloatingLiteral), Literal(Literal) {
    this->setValue(Value(std::stod(std::string(Literal.getLiteralData()))));
  }

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_FloatingLiteral;
  }

  Token getLiteral() { return Literal; }

  static FloatingLiteral *Create(Token Literal, SourceRange SrcRange) {
    return new FloatingLiteral(Literal, SrcRange);
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
      : Expr(EK_StringLiteral, SrcRange), Literal(Literal) {
    this->setValue(Value(std::string(Literal.getLiteralData())));
  }

public:
  static bool classof(const Expr *E) {
    return E->getKind() == EK_StringLiteral;
  }

  Token getLiteral() { return Literal; }

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

/// @brief Represents the result of a parse operation.
/// @tparam T The type of the result (Decl, Stmt, Expr).
template <typename T> struct ASTResult : private std::pair<T *, bool> {
  ASTResult(bool Invalid = false) : std::pair<T *, bool>(nullptr, Invalid) {}
  ASTResult(T *Ptr) : std::pair<T *, bool>(Ptr, true) {}
  T *get() { return this->first; }
  template <typename Ty> Ty *getAs() { return static_cast<Ty *>(get()); }
  bool isValid() { return this->second; }
  ASTResult &operator=(T *RHS) {
    this->first = RHS;
    this->second = true;
    return *this;
  }
};

using DeclResult = ASTResult<Decl>;
using StmtResult = ASTResult<Stmt>;
using ExprResult = ASTResult<Expr>;

} // namespace cawk
