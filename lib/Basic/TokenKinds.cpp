#include "Basic/TokenKinds.h"

#include <array>
#include <string_view>

using namespace cawk;

constexpr std::array<std::string_view, tok::NUM_TOKENS + 1> TokNames = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "Basic/TokenKinds.def"
    ""};

std::string_view tok::GetTokenName(TokenKind Kind) { return TokNames[Kind]; }

std::string_view tok::GetKeywordSpelling(TokenKind Kind) {
  switch (Kind) {
#define KEYWORD(ID, FLAG)                                                      \
  case kw_##ID:                                                                \
    return #ID;
#include "Basic/TokenKinds.def"
  default:
    break;
  }

  return {};
}
