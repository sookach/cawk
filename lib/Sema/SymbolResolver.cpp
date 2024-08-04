#include "Sema/SymbolResolver.h"

#include "Support/Support.h"

using namespace cawk;

bool SymbolResolver::visit(FunctionDecl *F) {
  if (!FunctionResolutions.try_emplace(std::string(F->getName()), F).second ||
      GlobalResolutions.contains(std::string(F->getName())))
    return false;

  LocalSymbols.clear();
  LocalResolutions.clear();
  return true;
}

bool SymbolResolver::visit(ParamVarDecl *P) {
  if (!LocalResolutions.try_emplace(std::string(P->getName()),
                                    DeclRefExpr::Create(P->getIdentifier())))
    return false;
  return true;
}

bool SymbolResolver::visit(RuleDecl *R) {
  LocalSymbols.clear();
  LocalResolutions.clear();
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(R->getPattern()))
    R->setPattern(resolve(D));
  return true;
}

bool SymbolResolver::visit(TranslationUnitDecl *T) { return true; }

bool SymbolResolver::visit(VarDecl *V) { return true; }

bool SymbolResolver::visit(BreakStmt *B) { return true; }

bool SymbolResolver::visit(CompoundStmt *C) { return true; }

bool SymbolResolver::visit(ContinueStmt *C) { return true; }

bool SymbolResolver::visit(DeleteStmt *D) {
  if (DeclRefExpr *DRE = dyn_cast_or_null<DeclRefExpr>(D->getArgument()))
    D->setArgument(resolve(DRE));
  return true;
}

bool SymbolResolver::visit(DoStmt *D) { return true; }

bool SymbolResolver::visit(ExitStmt *E) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(E->getValue()))
    E->setValue(resolve(D));
  return true;
}

bool SymbolResolver::visit(ForStmt *F) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(F->getCond()))
    F->setCond(resolve(D));
  return true;
}

bool SymbolResolver::visit(ForRangeStmt *F) {
  F->setLoopVar(resolve(F->getLoopVar()));
  F->setRange(resolve(F->getRange()));
  return true;
}

bool SymbolResolver::visit(IfStmt *I) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(I->getCond()))
    I->setCond(resolve(D));
  return true;
}

bool SymbolResolver::visit(NextStmt *N) { return true; }

bool SymbolResolver::visit(NextfileStmt *N) { return true; }

bool SymbolResolver::visit(PrintStmt *P) {
  for (int I = 0; I != std::size(P->getArgs()); ++I)
    if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(P->getArgs()[I]))
      P->setArg(I, resolve(D));
  return true;
}

bool SymbolResolver::visit(ReturnStmt *R) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(R->getValue()))
    R->setValue(D);
  return true;
}

bool SymbolResolver::visit(ValueStmt *V) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(V->getValue()))
    V->setValue(D);
  return true;
}

bool SymbolResolver::visit(WhileStmt *W) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(W->getCond()))
    W->setCond(D);
  return true;
}

bool SymbolResolver::visit(ArraySubscriptExpr *A) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(A->getLHS()))
    A->setLHS(D);
  for (int I = 0; I != std::size(A->getRHS()); ++I)
    if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(A->getRHS()[I]))
      A->setRHS(I, D);
  return true;
}

bool SymbolResolver::visit(BinaryOperator *B) {
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(B->getLHS()))
    B->setLHS(D);
  if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(B->getRHS()))
    B->setRHS(D);
  return true;
}

bool SymbolResolver::visit(CallExpr *C) {
  assert(isa<DeclRefExpr>(C->getCallee()));
  std::string Name(static_cast<DeclRefExpr *>(C->getCallee())->getName());
  if (FunctionResolutions.contains(Name)) {
    C->setFunction(FunctionResolutions[Name]);
  } else {
    UnresolvedSymbols.push_back(C);
  }

  for (int I = 0; I != std::size(C->getArgs()); ++I)
    if (DeclRefExpr *D = dyn_cast_or_null<DeclRefExpr>(C->getArgs()[I]))
      C->setArg(I, D);

  return true;
}

bool SymbolResolver::visit(DeclRefExpr *D) { return true; }

bool SymbolResolver::visit(FloatingLiteral *F) { return true; }

bool SymbolResolver::visit(RegexLiteral *R) { return true; }

bool SymbolResolver::visit(StringLiteral *S) { return true; }

bool SymbolResolver::visit(UnaryOperator *U) { return true; }

bool SymbolResolver::check(TranslationUnitDecl *T) {
  if (!visit(T))
    return false;

  for (CallExpr *C : UnresolvedSymbols)
    if (std::string Name(ptr_cast<DeclRefExpr>(C->getCallee())->getName());
        !FunctionResolutions.contains(Name))
      return false;
    else
      C->setFunction(FunctionResolutions[Name]);

  return true;
}

DeclRefExpr *SymbolResolver::resolve(DeclRefExpr *D) {
  std::string Name(D->getName());
  if (LocalResolutions.contains(Name))
    return LocalResolutions.try_emplace(Name, D).first->second;
  assert(!FunctionResolutions.contains(Name));
  return GlobalResolutions.try_emplace(Name, D).first->second;
}