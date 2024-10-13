#include "cawk/Basic/OperatorPrecedence.h"
#include "cawk/Basic/TokenKinds.h"

using namespace cawk;

prec::Level cawk::getBinOpPrecedence(tok::TokenKind Kind) {
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
  case tok::identifier:
  case tok::numeric_constant:
  case tok::string_literal:
  case tok::plusplus:
  case tok::minusminus:
  case tok::exclaim:
  case tok::l_paren:
    return prec::StringConcat;
  case tok::plus:
  case tok::minus:
    return prec::Additive;
  case tok::star:
  case tok::slash:
  case tok::percent:
    return prec::Multiplicative;
  case tok::caret:
  case tok::starstar:
    return prec::Exponentiation;
  }
}
