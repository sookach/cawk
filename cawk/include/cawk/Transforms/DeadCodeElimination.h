#include "cawk/AST/AST.h"
#include "cawk/AST/ASTVisitor.h"
#include "cawk/Support/Support.h"

namespace cawk {
class DCEPass : public ASTVisitor<DCEPass, trav::Preorder, true> {
  friend class ASTVisitor<DCEPass, trav::Preorder, true>;

  bool visit(CompoundStmt *S) {
    std::vector<Stmt *> Body;
    for (Stmt *S : S->getBody())
      if (!isa<NullStmt>(S))
        Body.push_back(S);
    S->setBody(Body);
    return true;
  }

  bool visit(DoStmt *S) {
    if (isa<NullStmt>(S->getBody()))
      S->setBody(nullptr);
    return true;
  }

  bool visit(ForStmt *S) {
    if (isa<NullStmt>(S->getBody()))
      S->setBody(nullptr);
    return true;
  }

  bool visit(ForRangeStmt *S) {
    if (isa<NullStmt>(S->getBody()))
      S->setBody(nullptr);
    return true;
  }

  bool visit(IfStmt *S) {
    if (isa<NullStmt>(S->getThen()))
      S->setThen(nullptr);
    if (S->getElse() != nullptr && isa<NullStmt>(S->getElse()))
      S->setElse(nullptr);
    return true;
  }

  bool visit(WhileStmt *S) {
    if (isa<NullStmt>(S->getBody()))
      S->setBody(nullptr);
    return true;
  }
};
} // namespace cawk