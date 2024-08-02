#include "Sema/Resolver.h"

#include "Support/Support.h"

using namespace cawk;

bool Resolver::visit(FunctionDecl *F) {
  if (!GlobalSymbols.try_emplace(std::string(F->getName()), F))
    return false;

  LocalSymbols.clear();
  return true;
}

bool Resolver::visit(ParamVarDecl *P) {
  if (!LocalSymbols.try_emplace(std::string(P->getName())), P)
    return false;
  return true;
}

bool Resolver::visit(RuleDecl *R) { return true; }

bool Resolver::visit(TranslationUnitDecl *T) { return true; }

bool Resolver::visit(VarDecl *V) { return true; }

bool Resolver::visit(BreakStmt *B) { return true; }

bool Resolver::visit(CompoundStmt *C) { return true; }

bool Resolver::visit(ContinueStmt *C) { return true; }

bool Resolver::visit(DeleteStmt *D) { return true; }

bool Resolver::visit(DoStmt *D) { return true; }

bool Resolver::visit(ExitStmt *E) { return true; }

bool Resolver::visit(ForStmt *F) { return true; }

bool Resolver::visit(ForRangeStmt *F) { return true; }

bool Resolver::visit(IfStmt *I) { return true; }

bool Resolver::visit(NextStmt *N) { return true; }

bool Resolver::visit(NextfileStmt *N) { return true; }

bool Resolver::visit(PrintStmt *P) { return true; }

bool Resolver::visit(ReturnStmt *R) { return true; }

bool Resolver::visit(ValueStmt *V) { return true; }

bool Resolver::visit(WhileStmt *W) { return true; }

bool Resolver::visit(ArraySubscriptExpr *A) { return true; }

bool Resolver::visit(BinaryOperator *B) { return true; }

bool Resolver::visit(CallExpr *C) { return true; }

bool Resolver::visit(DeclRefExpr *D) {
  if (LocalSymbols.contains(std::string(D->getName()))) {
    
  }
  if (GlobalSymbols.contains(std::string(D->getName())) &&
      !GlobalSymbols[std::string(D->getName())].is(GlobalSymbol::Variable))
    return false;
  GlobalSymbols.try_emplace(std::string(D->getName()), D);
  return true;
}

bool Resolver::visit(FloatingLiteral *F) { return true; }

bool Resolver::visit(RegexLiteral *R) { return true; }

bool Resolver::visit(StringLiteral *S) { return true; }

bool Resolver::visit(UnaryOperator *U) { return true; }

bool Resolver::check(TranslationUnitDecl *T) { return visit(T); }