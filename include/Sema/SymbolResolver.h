#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "Basic/Diagnostic.h"
#include "Exec/Value.h"

#include <iostream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>

namespace cawk {
class SymbolResolver : ASTVisitor<SymbolResolver, trav::Preorder, true> {
  friend class ASTVisitor<SymbolResolver, trav::Preorder, true>;
  Diagnostic &Diags;

  std::unordered_map<std::string, Value *> GlobalResolutions;
  std::unordered_map<std::string, Value *> LocalResolutions;
  std::unordered_map<std::string, ParamVarDecl *> LocalSymbols;
  std::vector<CallExpr *> UnresolvedSymbols;

public:
  SymbolResolver(Diagnostic &Diags) : Diags(Diags) {}
  bool check(TranslationUnitDecl *T);
  auto &getGlobals() { return GlobalResolutions; }

private:
  bool visit(FunctionDecl *F);
  bool visit(ParamVarDecl *P);
  bool visit(RuleDecl *R);
  bool visit(DeclRefExpr *D);
  void resolve(DeclRefExpr *D);
};

} // namespace cawk