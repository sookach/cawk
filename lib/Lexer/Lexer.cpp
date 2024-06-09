#include "Lexer/Lexer.h"
#include "Basic/TokenKinds.h"

namespace charinfo {
inline bool IsWhitespace(char c) {
  switch (c) {
  default:
    return false;
  case ' ':
  case '\t':
  case '\f':
  case '\v':
  case '\r':
    //  case '\n': Newline have syntactically important in AWK
    return true;
  }
}

inline bool IsDigit(char c) {
  switch (c) {
  default:
    return false;
  case '0' ... '9':
    return true;
  }
}

inline bool IsLetter(char c) {
  switch (c) {
  default:
    return false;
  case 'A' ... 'Z':
  case 'a' ... 'z':
    return true;
  }
}
} // namespace charinfo

namespace cawk {

std::string_view::const_iterator Lexer::GetBufferPtr() const {
  return BufferPtr;
}

void Lexer::SetBufferPtr(std::string_view::const_iterator Ptr) {
  BufferPtr = Ptr;
}

void Lexer::Undo() { BufferPtr = BufferPrev; }

void Lexer::FormToken(Token &T, std::string_view::const_iterator End,
                      tok::TokenKind Kind) {
  T.Kind = Kind;
  T.Ptr = BufferPtr;
  T.Length = End - BufferPtr;
  T.Kind = Kind;
  BufferPtr = End;
}

void Lexer::Next(Token &T, bool Regex) {
  BufferPrev = BufferPtr;

  for (; BufferPtr != BufferEnd && charinfo::IsWhitespace(*BufferPtr);
       ++BufferPtr)
    ;

  if (BufferPtr == BufferEnd) {
    T.Kind = tok::eof;
    return;
  }

  if (charinfo::IsLetter(*BufferPtr)) {
    auto End = BufferPtr + 1;
    for (; charinfo::IsLetter(*End); ++End)
      ;
    std::string_view Name(BufferPtr, End);
    tok::TokenKind Kind =
        Keywords.contains(Name) ? Keywords.at(Name) : tok::identifier;
    FormToken(T, End, Kind);
    return;
  }

  if (charinfo::IsDigit(*BufferPtr)) {
    auto End = BufferPtr + 1;
    for (; charinfo::IsDigit(*End); ++End)
      ;
    if (*BufferPtr == '.')
      for (++BufferPtr; charinfo::IsDigit(*BufferPtr); ++BufferPtr)
        ;
    FormToken(T, End, tok::numeric_constant);
    return;
  }

  if (*BufferPtr == '"') {
    auto End = BufferPtr + 1;
    for (; End != BufferEnd && *End != '"'; ++End)
      if (*End == '\\')
        ++End;
    FormToken(T, End + 1, tok::string_literal);
    return;
  }

  if (*BufferPtr == '/' && Regex) {
    auto End = BufferPtr + 1;
    for (; End != BufferEnd && *End != '/'; ++End)
      if (*End == '\\')
        ++End;
    FormToken(T, End + 1, tok::regex_literal);
    return;
  }

  switch (*BufferPtr) {
#define CASE(CH, TOK)                                                          \
  case CH:                                                                     \
    FormToken(T, BufferPtr + 1, TOK);                                          \
    break
    CASE('[', tok::l_square);
    CASE(']', tok::r_square);
    CASE('(', tok::l_paren);
    CASE(')', tok::r_paren);
    CASE('?', tok::question);
    CASE(':', tok::colon);
    CASE(';', tok::semi);
    CASE(',', tok::comma);
    CASE('$', tok::dollar);
    CASE('/', tok::slash);
    CASE('~', tok::tilde);
#undef CASE
  case '&':
    if (*(BufferPtr + 1) == '&')
      FormToken(T, BufferPtr + 2, tok::ampamp);
    else
      FormToken(T, BufferPtr + 1, tok::amp);
    break;
  case '|':
    if (*(BufferPtr + 1) == '|')
      FormToken(T, BufferPtr + 2, tok::pipepipe);
    else
      FormToken(T, BufferPtr + 1, tok::pipe);
    break;
  case '=':
    if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::equalequal);
    else
      FormToken(T, BufferPtr + 1, tok::equal);
    break;
  case '!':
    if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::exclaimequal);
    else if (*(BufferPtr + 1) == '~')
      FormToken(T, BufferPtr + 2, tok::exclaimtilde);
    else
      FormToken(T, BufferPtr + 1, tok::exclaim);
    break;
  case '<':
    if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::lessequal);
    else
      FormToken(T, BufferPtr + 1, tok::less);
    break;
  case '>':
    if (*(BufferPtr + 1) == '>')
      FormToken(T, BufferPtr + 2, tok::greatergreater);
    else if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::greaterequal);
    else
      FormToken(T, BufferPtr + 1, tok::greater);
    break;
  case '+':
    if (*(BufferPtr + 1) == '+')
      FormToken(T, BufferPtr + 2, tok::plusplus);
    else if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::plusequal);
    else
      FormToken(T, BufferPtr + 1, tok::plus);
    break;
  case '-':
    if (*(BufferPtr + 1) == '-')
      FormToken(T, BufferPtr + 2, tok::minusminus);
    else if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::minusequal);
    else
      FormToken(T, BufferPtr + 1, tok::minus);
    break;
  case '*':
    if (*(BufferPtr + 1) == '*')
      FormToken(T, BufferPtr + 2, tok::starstar);
    else if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::starequal);
    else
      FormToken(T, BufferPtr + 1, tok::star);
    break;
  case '^':
    if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::caretequal);
    else
      FormToken(T, BufferPtr + 1, tok::caret);
    break;
  case '%':
    if (*(BufferPtr + 1) == '=')
      FormToken(T, BufferPtr + 2, tok::percentequal);
    else
      FormToken(T, BufferPtr + 1, tok::percent);
    break;
  case '\n':
    FormToken(T, BufferPtr + 1, tok::newline);
    // condense successive newlines
    for (; BufferPtr != BufferEnd && *BufferPtr == '\n'; ++BufferPtr)
      ;
    break;
  default:
    FormToken(T, BufferPtr + 1, tok::unknown);
  }
}

} // namespace cawk
