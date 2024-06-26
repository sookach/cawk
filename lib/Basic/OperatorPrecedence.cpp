#include "Basic/OperatorPrecedence.h"
#include "Basic/TokenKinds.h"

namespace cawk {
prec::Level GetBinOpPrecedence(tok::TokenKind Kind) {
  switch (Kind) {
  default:
    return prec::Unknown;
  case tok::equal:
  case tok::plusequal:
  case tok::minusequal:
  case tok::starequal:
  case tok::slashequal:
  case tok::percentequal:
  case tok::caretequal:
  case tok::starstarequal:
    return prec::Assignment;
  case tok::question:
    return prec::Conditional;
  case tok::pipepipe:
    return prec::LogicalOr;
  case tok::ampamp:
    return prec::LogicalAnd;
  case tok::kw_in:
    return prec::Membership;
  case tok::tilde:
  case tok::exclaimtilde:
    return prec::Matching;
  case tok::less:
  case tok::lessequal:
  case tok::equalequal:
  case tok::exclaimequal:
  case tok::greater:
  case tok::greaterequal:
    return prec::Relational;
    // case tok::greater:
  case tok::greatergreater:
  case tok::pipe:
  case tok::pipeamp:
    return prec::Redirect;
  }
}
} // namespace cawk
