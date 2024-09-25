//===- Token.h - Token interface --------------------------------*- C++ -*-===//
//
//  This file defines the Token interface.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "Basic/TokenKinds.h"
#include <cassert>
#include <string_view>

namespace cawk {
class Token {
  friend class Lexer;

  /// Ptr - The start position of the Token the input buffer.
  std::string_view::const_iterator Ptr;

  /// Length - The length of the Token.
  std::size_t Length;

  /// Kind - The kind of the Token.
  tok::TokenKind Kind;

  /// Line - The line number of the Token.
  std::size_t Line;

public:
  /// getKind - Returns the kind of the Token.
  tok::TokenKind getKind() const { return Kind; }

  /// getLength - Returns the length of the Token.
  std::size_t getLength() const { return Length; }

  /// getLine - Returns the line number of the Token.
  std::size_t getLine() const { return Line; }

  /// is - Returns true if the Token is of any of the given kinds.
  template <typename... Ts> bool is(Ts... Ks) const {
    return (false || ... || (Kind == Ks));
  }

  /// getIdentifier - Returns the characters of the Token, checking that the
  /// Token is an identifier.
  std::string_view getIdentifier() const {
    assert(is(tok::identifier) && "Cannot get identifier of non-identifier");
    return getRawData();
  }

  /// getLiteralData - Returns the characters of the Token, checking that the
  /// Token is a literal.
  std::string_view getLiteralData() const {
    assert(is(tok::numeric_constant, tok::string_literal) &&
           "Cannot get literal data of non-literal");
    return getRawData();
  }

  /// getRawData - Returns the characters of the Token.
  std::string_view getRawData() const { return {Ptr, Length}; }
};
} // namespace cawk