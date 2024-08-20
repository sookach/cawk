#pragma once

#include "AST/ASTVisitor.h"
#include "Basic/Diagnostic.h"
#include "Sema/FunctionSymbol.h"
#include "Support/StringMap.h"

#include <vector>

namespace cawk {
class SemaLValue : public ASTVisitor<SemaLValue, trav::Postorder, true> {
  friend class ASTVisitor<SemaLValue, trav::Postorder, true>;
  Diagnostic &Diags;

public:
  SemaLValue(Diagnostic &Diags) : Diags(Diags) {}
  bool check(TranslationUnitDecl *T);

private:
  bool visit(ForRangeStmt *F);

  bool visit(ArraySubscriptExpr *A);
  bool visit(BinaryOperator *B);
  bool visit(DeclRefExpr *D);
  bool visit(DeleteStmt *D);
};
} // namespace cawk