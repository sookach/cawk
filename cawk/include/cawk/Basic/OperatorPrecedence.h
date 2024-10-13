#pragma once

#include "cawk/Basic/TokenKinds.h"

namespace cawk {
namespace prec {
enum Level {
  Unknown = 0,         // Not binary operator.
  Assignment = 1,      // =, *=, /=, %=, +=, -=, <<=, >>=, &=, ^=, |=
  Conditional = 2,     // ?
  LogicalOr = 3,       // ||
  LogicalAnd = 4,      // &&
  Membership = 5,      // in
  Matching = 6,        // ~, !~
  Relational = 7,      // >=, <=
  Redirect = 7,        // >>, |, |&
  StringConcat = 8,    // No designated operator.
  Additive = 9,        // -, +
  Multiplicative = 10, // *, /, %
  Exponentiation = 11, // ^, **
  Maximum
};
}

prec::Level getBinOpPrecedence(tok::TokenKind Kind);
} // namespace cawk
