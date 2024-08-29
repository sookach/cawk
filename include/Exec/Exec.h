#pragma once

#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "Exec/IO.h"
#include "Exec/Value.h"
#include "Sema/FunctionSymbol.h"
#include "Support/StringMap.h"
#include "Support/Support.h"

#include <cstddef>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace cawk {
class Exec : public ASTVisitor<Exec, trav::None, true> {
  friend class ASTVisitor<Exec, trav::None, true>;

  static std::unique_ptr<Exec> Process;

  TranslationUnitDecl *AST;
  std::vector<Value> Fields;
  std::vector<InputFile> Inputs;
  std::unordered_map<std::string, Value *> BuiltinVariables;
  Value NullValue;
  CallExpr *ParentFunction;
  std::uint32_t NestedLevel = 0;
  std::uint32_t CallLevel = 0;
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
  void addInput(std::string Filepath);

  void operator()();

  bool visit(TranslationUnitDecl *T);
  bool visit(RuleDecl *R);

  bool visit(BreakStmt *B);
  bool visit(ContinueStmt *C);
  bool visit(CompoundStmt *C);
  bool visit(DeleteStmt *D);
  bool visit(DoStmt *D);
  bool visit(ExitStmt *E);
  bool visit(ForStmt *F);
  bool visit(ForRangeStmt *F);
  bool visit(IfStmt *I);
  bool visit(NextStmt *N);
  bool visit(NextfileStmt *N);
  bool visit(PrintStmt *P);
  bool visit(ReturnStmt *R);
  bool visit(ValueStmt *V);
  bool visit(WhileStmt *W);

  bool visit(ArraySubscriptExpr *A);
  bool visit(BinaryOperator *B);
  bool visit(CallExpr *C);
  bool visit(DeclRefExpr *D);
  bool visit(FloatingLiteral *F);
  bool visit(RegexLiteral *R);
  bool visit(StringLiteral *S);
  bool visit(UnaryOperator *U);

  Value &getField(std::size_t I);

  static bool isBuiltin(tok::TokenKind Kind);
  static Value execBuiltin(tok::TokenKind Kind, std::vector<Value> Args);

  bool isEarlyExit();
};

} // namespace cawk
