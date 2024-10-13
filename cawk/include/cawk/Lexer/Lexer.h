//===- Lexer.h - CAWK Language Lexer ----------------------------*- C++ -*-===//
//
//  This file defines the Lexer interface.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "cawk/Basic/Diagnostic.h"
#include "cawk/Basic/TokenKinds.h"
#include "cawk/Exec/IO.h"
#include "cawk/Token.h"

#include <string_view>
#include <unordered_map>

namespace cawk {
class Lexer {
  using Pointer = std::string_view::const_iterator;

  /// Diags - The diagnostic engine.
  Diagnostic &Diags;

  /// BufferStart - The start of the input buffer.
  Pointer BufferStart;

  /// BufferEnd - The end of the input buffer.
  Pointer BufferEnd;

  /// BufferPtr - The current position in the input buffer.
  Pointer BufferPtr;

  /// BufferPrev - The previous position in the input buffer.
  Pointer BufferPrev;

  /// Keywords - The set of keywords.
  std::unordered_map<std::string_view, tok::TokenKind> Keywords;

  /// Line - The current line number.
  std::size_t Line = 1;

public:
  Lexer(std::string_view Buffer, Diagnostic &Diags)
      : Diags(Diags), BufferStart(std::cbegin(Buffer)),
        BufferEnd(std::cend(Buffer)), BufferPtr(BufferStart) {
#define KEYWORD(ID, KEY) Keywords.emplace(#ID, tok::kw_##ID);
#include "Basic/TokenKinds.def"
#undef KEYWORD
  }

  Pointer getBufferPtr() const;
  void setBufferPtr(Pointer Ptr);
  void next(Token &Result);
  void lexRegexLiteral(Token &Result);
  Token peek(std::size_t N = 1);
  void undo();

  void formSpaceToken(Token &Result, Pointer It);

private:
  void formTokenWithChars(Token &Result, Pointer TokEnd, tok::TokenKind Kind);
  void lexIdentifier(Token &Result);
  void lexNumericConstant(Token &Result);
  void lexStringLiteral(Token &Result);

  std::size_t getLine() const { return Line; }
};
} // namespace cawk
