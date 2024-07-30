#pragma once

#include "AST/AST.h"
#include "Exec/IO.h"
#include "Exec/Value.h"
#include "Support/StringMap.h"
#include "Support/Support.h"

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace cawk {
class Exec {
  struct FunctionDeclEmplace {
    template <typename T1, typename... T2>
    void operator()(auto &Table, T1 Key, T2 &&...Args) {
      Table.emplace_back(Key, nullptr);
    }
  };
  static std::unique_ptr<Exec> Process;

  TranslationUnitDecl *AST;
  StringMap<FunctionDecl *, FunctionDeclEmplace> Functions;
  StringMap<Value> Globals;
  StringMap<Value> Locals;
  std::vector<Value> Fields;
  std::vector<InputFile> Inputs;
  Value NullValue;
  Value ReturnValue;
  std::uint32_t NestedLevel = 0;
  std::uint32_t CallLevel = 0;
  bool IsBegin = true;
  bool IsEnd = false;
  bool ShouldBreak = false;
  bool ShouldContinue = false;
  bool ShouldReturn = false;
  bool SkipToNext = false;
  bool SkipToNextfile = false;

private:
  Exec() = default;

public:
  static void load(TranslationUnitDecl *T, std::vector<std::string> Filepaths);
  static void exec();

private:
  void addFunction(const FunctionDecl *F);
  void addInput(std::string Filepath);

  void operator()();

  void visit(TranslationUnitDecl *T);
  void visit(RuleDecl *R);

  void visit(Stmt *S);
  void visit(BreakStmt *B);
  void visit(ContinueStmt *C);
  void visit(CompoundStmt *C);
  void visit(DeleteStmt *D);
  void visit(DoStmt *D);
  void visit(ExitStmt *E);
  void visit(ForStmt *F);
  void visit(ForRangeStmt *F);
  void visit(IfStmt *I);
  void visit(NextStmt *N);
  void visit(NextfileStmt *N);
  void visit(PrintStmt *P);
  void visit(ReturnStmt *R);
  void visit(ValueStmt *V);
  void visit(WhileStmt *W);

  Value visit(Expr *E);
  Value &visit(ArraySubscriptExpr *A);
  Value visit(BinaryOperator *B);
  Value visit(CallExpr *C);
  Value visit(DeclRefExpr *D);
  Value visit(FloatingLiteral *F);
  Value visit(RegexLiteral *R);
  Value visit(StringLiteral *S);
  Value visit(UnaryOperator *U);

  Value &getValue(std::string_view Name);
  Value &getValue(DeclRefExpr *E);
  void setValue(std::string_view Name, Value V);
  void setValue(DeclRefExpr *D, Value V);
  Value &getField(std::size_t I);

  static bool isBuiltin(tok::TokenKind Kind);
  static Value execBuiltin(tok::TokenKind Kind, std::vector<Value> Args);

  bool isEarlyExit();
};

} // namespace cawk
