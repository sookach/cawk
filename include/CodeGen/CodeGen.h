#include "AST/AST.h"
#include "Basic/TokenKinds.h"
#include "Lexer/Lexer.h"
#include "Support/Support.h"

#include <ostream>
#include <ranges>
#include <vector>

namespace cawk {

class CodeGen {
  template <typename V> using StringMap = std::unordered_map<std::string, V>;

  std::ostream &OS;
  std::vector<std::string> FunctionSignatures;
  std::vector<std::string> GlobalVars;
  unsigned NumRules = 0;

public:
  CodeGen(std::ostream &OS, std::vector<std::string> FunctionSignatures,
          std::vector<std::string> GlobalVars)
      : OS(OS), FunctionSignatures(FunctionSignatures), GlobalVars(GlobalVars) {
  }

private:
  void Print(tok::TokenKind K) { OS << tok::GetSpelling(K); }

  void Print(std::string_view S) { OS << S; }

  void Print(Token T) { OS << T.GetLiteralData(); }

  void Print(const Decl *D) {
    switch (D->GetKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Decl::DK_##KIND:                                                        \
    return Print(ptr_cast<CLASS>(D))
      CASE(TranslationUnit, TranslationUnitDecl);
      CASE(Rule, RuleDecl);
      CASE(Function, FunctionDecl);
      CASE(Var, VarDecl);
      CASE(ParamVar, ParamVarDecl);
#undef CASE
    }
  }

  void Print(const TranslationUnitDecl *T) {
    for (const Decl *D : T->GetDecls())
      Print(D);
  }

  void Print(const RuleDecl *R) {
    Print(std::format("void Rule{}()", NumRules++));
    Print(CompoundStmt::Create(
        {IfStmt::Create(R->GetPattern(), R->GetAction(), nullptr)}));
  }

  void Print(const FunctionDecl *F) {
    Print(F->GetIdentifier());

    Print(tok::l_paren);

    if (!std::empty(F->GetParams())) {
      Print(F->GetParams().front());
      for (const ParamVarDecl *P : F->GetParams() | std::views::drop(1)) {
        Print(tok::comma);
        Print(P);
      }
    }

    Print(tok::r_paren);

    Print(F->GetBody());
  }

  void Print(const Stmt *S) {
    switch (S->GetKind()) {
#define CASE(KIND, CLASS)                                                      \
  case Stmt::SK_##KIND:                                                        \
    return Print(ptr_cast<CLASS>(S))
      CASE(Break, BreakStmt);
      CASE(Continue, ContinueStmt);
      CASE(Compound, CompoundStmt);
      CASE(Delete, DeleteStmt);
      CASE(Do, DoStmt);
      CASE(Exit, ExitStmt);
      CASE(For, ForStmt);
      CASE(ForRange, ForRangeStmt);
      CASE(If, IfStmt);
      CASE(Next, NextStmt);
      CASE(Nextfile, NextfileStmt);
      CASE(Print, PrintStmt);
      CASE(Return, ReturnStmt);
      CASE(Value, ValueStmt);
      CASE(While, WhileStmt);
#undef CASE
    }
  }

#if 0
#endif
};

} // namespace cawk
