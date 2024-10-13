#pragma once

#include "IRToken.h"

#include <cctype>
#include <string>
#include <string_view>

namespace cir {
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

  /// Keywords - The set of keywords.
  std::unordered_map<std::string_view, tok::TokenKind> Keywords;

  /// Line - The current line number.
  std::size_t Line = 1;

public:
  Lexer(std::string_view Buffer)
      : BufferStart(std::cbegin(Buffer)), BufferEnd(std::cend(Buffer)) {
#define KEYWORD(ID, KEY) Keywords.emplace(#ID, tok::kw_##ID);
#include "TokenKinds.def"
  }

  /// isEOF - Return true if the current pointer is at the end of the buffer.
  bool isEOF() const { return BufferPtr == BufferEnd; }

  /// BufferPtr - Return the current pointer.
  Pointer BufferPtr() const { return BufferPtr; }

  /// formToken - Form a token with the given kind.
  void formToken(Token &Result, Pointer TokEnd, tok::TokenKind Kind) {
    Result = Token(Kind, std::string_view(BufferPtr, BufferEnd), Line);
    BufferPtr = TokEnd;
  }

  /// formKeyword - Form an keyword token.
  void formKeyword(Token &Result) {
    auto Next = std::find_if_not(BufferPtr, BufferEnd, std::isalnum);
    auto Keyword = std::string_view(BufferPtr, Next - BufferPtr);
    assert(Keywords.contains(Keyword));
    formToken(Keywords.at(Keyword), Result);
    BufferPtr = Next;
  }

  /// next - Return the next token from the input.
  void next(Token &Result) {
    BufferPtr();
    if (isEOF())
      return formToken(tok::eof, Result);

    if (std::isalpha(*BufferPtr))
      return formKeyword(Result);

    if (*BufferPtr == '@')
      return formGlobal(Result);

    if (*BufferPtr == '%')
      return formLocal(Result);

    if (*BufferPtr == '#')
      return formConstant(Result);

    if (std::isdigit(*BufferPtr))
      return formNumber(Result);

    if (*BufferPtr == '"')
      return formString(Result);

    switch (*BufferPtr) {
#define CASE(CH, TOK)                                                          \
  case CH:                                                                     \
    return formToken(Result, BufferPtr + 1, TOK);
      CASE('(', tok::l_paren);
      CASE(')', tok::r_paren);
      CASE('{', tok::l_brace);
      CASE('}', tok::r_brace);
      CASE('=', tok::equal);
      CASE(',', tok::comma);
      CASE(';', tok::semicolon);
#undef CASE
    default:
      return formToken(tok::unknown, Result);
    }
  }
};

} // namespace cir