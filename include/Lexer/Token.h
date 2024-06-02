#include "Basic/TokenKinds.h"
#include <cassert>
#include <string_view>

namespace cawk {
class Token {
  friend class Lexer;

  std::string_view::const_iterator Ptr;
  std::size_t Length;
  tok::TokenKind Kind;

public:
  tok::TokenKind GetKind() const { return Kind; }

  std::size_t GetLength() const { return Length; }

  template <typename... Ts> bool Is(Ts... Ks) const {
    return (false || ... || (Kind == Ks));
  }

  std::string_view GetIdentifier() const {
    assert(Is(tok::identifier) && "Cannot get identifier of non-identifier");
    return {Ptr, Length};
  }

  std::string_view GetLiteralData() const {
    assert(Is(tok::numeric_constant, tok::string_literal) &&
           "Cannot get literal data of non-literal");
    return {Ptr, Length};
  }
};
} // namespace cawk
