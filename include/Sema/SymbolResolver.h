#include "AST/AST.h"
#include "AST/ASTVisitor.h"

#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>

namespace cawk {
class SymbolResolver : ASTVisitor<SymbolResolver, trav::Preorder, true> {
  friend class ASTVisitor<SymbolResolver, trav::Preorder, true>;

  std::unordered_map<std::string, FunctionDecl *> FunctionResolutions;
  std::unordered_map<std::string, DeclRefExpr *> GlobalResolutions;
  std::unordered_map<std::string, DeclRefExpr *> LocalResolutions;
  std::unordered_set<std::string> LocalSymbols;
  std::vector<CallExpr *> UnresolvedSymbols;

public:
  bool check(TranslationUnitDecl *T);

private:
  bool visit(FunctionDecl *F);
  bool visit(ParamVarDecl *P);
  bool visit(RuleDecl *R);
  bool visit(TranslationUnitDecl *T);
  bool visit(VarDecl *V);

  bool visit(BreakStmt *B);
  bool visit(CompoundStmt *C);
  bool visit(ContinueStmt *C);
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

  DeclRefExpr *resolve(DeclRefExpr *D);
};

} // namespace cawk