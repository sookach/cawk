#pragma once

#include <cstddef>
#include <string_view>

namespace cir {
namespace tok {
enum TokenType {
#define TOKEN(ID) ID,
#include "TokenKinds.def"
};
} // namespace tok

class Token {
  TokenType Kind;
  std::string_view Text;
  std::size_t Line;

public:
  Token(TokenType Kind, std::string_view Text, std::size_t Line)
      : Kind(Kind), Text(Text), Line(Line) {}

  TokenType getKind() const { return Kind; }
  std::string_view getText() const { return Text; }
  std::size_t getLine() const { return Line; }
};
} // namespace cir