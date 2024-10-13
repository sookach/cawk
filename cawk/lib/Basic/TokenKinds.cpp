#include "cawk/Basic/TokenKinds.h"

#include <array>
#include <string_view>

using namespace cawk;

constexpr std::array<std::string_view, tok::NUM_TOKENS + 1> TokNames = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "Basic/TokenKinds.def"
    ""};

std::string_view tok::getTokenName(TokenKind Kind) { return TokNames[Kind]; }

std::string_view tok::getSpelling(TokenKind Kind) {
  switch (Kind) {
#define PUNCTUATOR(ID, SP)                                                     \
  case ID:                                                                     \
    return #SP;
#define KEYWORD(ID, FLAG)                                                      \
  case kw_##ID:                                                                \
    return #ID;
#include "Basic/TokenKinds.def"
  default:
    return {};
  }
}

std::string_view tok::getPunctuatorSpelling(TokenKind Kind) {
  switch (Kind) {
#define PUNCTUATOR(ID, SP)                                                     \
  case ID:                                                                     \
    return #SP;
#include "Basic/TokenKinds.def"
  default:
    break;
  }
  return {};
}

std::string_view tok::getKeywordSpelling(TokenKind Kind) {
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
