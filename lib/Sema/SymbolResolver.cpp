#include "Sema/SymbolResolver.h"

#include "Support/Support.h"

using namespace cawk;

bool SymbolResolver::visit(FunctionDecl *F) {
  if (GlobalResolutions.contains(std::string(F->getName())))
    return false;

  LocalSymbols.clear();
  LocalResolutions.clear();
  return true;
}

bool SymbolResolver::visit(ParamVarDecl *P) {
  if (!LocalResolutions.try_emplace(std::string(P->getName()), new Value())
           .second)
    return false;
  return true;
}

bool SymbolResolver::visit(RuleDecl *R) {
  LocalResolutions.clear();
  return true;
}

bool SymbolResolver::check(TranslationUnitDecl *T) {
  if (!traverse(T))
    return false;

  for (CallExpr *C : UnresolvedSymbols)
    if (std::string Name(ptr_cast<DeclRefExpr>(C->getCallee())->getName());
        !FunctionResolutions.contains(Name))
      return false;
    else
      C->setFunction(FunctionResolutions[Name]);

  return true;
}

bool SymbolResolver::visit(CallExpr *C) {
  resolve(C);
  return true;
}

bool SymbolResolver::visit(DeclRefExpr *D) {
  resolve(D);
  return true;
}

void SymbolResolver::resolve(CallExpr *C) {
  if (FunctionResolutions.contains(C->getName())) {
    C->setFunction(FunctionResolutions[C->getName()]);
  } else {
    UnresolvedSymbols.push_back(C);
  }
}

void SymbolResolver::resolve(DeclRefExpr *D) {
  if (D->getType() == type::function) {
  }
  std::string Name(D->getName());
  if (LocalResolutions.contains(Name)) {
    D->setValue(LocalResolutions[Name]);
  } else {
    assert(!FunctionResolutions.contains(Name));
    if (!GlobalResolutions.contains(Name))
      GlobalResolutions[Name] = new Value;
    D->setValue(GlobalResolutions[Name]);
  }
}