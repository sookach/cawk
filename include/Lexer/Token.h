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
  tok::TokenKind getKind() const { return Kind; }

  std::size_t getLength() const { return Length; }

  template <typename... Ts> bool is(Ts... Ks) const {
    return (false || ... || (Kind == Ks));
  }

  std::string_view getIdentifier() const {
    assert(is(tok::identifier) && "Cannot get identifier of non-identifier");
    return {Ptr, Length};
  }

  std::string_view getLiteralData() const {
    assert(is(tok::numeric_constant, tok::string_literal) &&
           "Cannot get literal data of non-literal");
    return {Ptr, Length};
  }
};
} // namespace cawk
