#include "AST/AST.h"
#include "AST/ASTVisitor.h"

#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>

namespace cawk {
class Resolver : ASTVisitor<Resolver, trav::Preorder, true> {
  friend class ASTVisitor<Resolver, trav::Preorder, true>;

  class GlobalSymbol {
  public:
    enum SymbolKind { Function, Variable };

  private:
    SymbolKind Kind;
    std::variant<std::variant<FunctionDecl *, DeclRefExpr *>> Symbol;

  public:
    GlobalSymbol(FunctionDecl *F) : Symbol(F), Kind(Function) {}

    GlobalSymbol(DeclRefExpr *D) : Symbol(D), Kind(Variable) {}

    bool is(SymbolKind K) { return Kind == K; }

    template <SymbolKind K> auto *get() {
      if (!is(K))
        return nullptr;

      if constexpr (K == Function)
        return std::get<FunctionDecl *>(Symbol);
      else
        return std::get<DeclRefExpr *>(Symbol);
    }
  };

  std::unordered_map<std::string, GlobalSymbol> GlobalSymbols;
  std::unordered_map<std::string, ParamVarDecl *> LocalSymbols;

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
};

} // namespace cawk