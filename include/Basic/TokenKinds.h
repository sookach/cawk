#pragma once

#include <string_view>

namespace cawk {
namespace tok {
enum TokenKind : unsigned short {
#define TOK(ID) ID,
#include "TokenKinds.def"
  NUM_TOKENS
};

std::string_view getTokenName(TokenKind Kind);
std::string_view getSpelling(TokenKind);
std::string_view getPunctuatorSpelling(TokenKind Kind);
std::string_view getKeywordSpelling(TokenKind Kind);
} // namespace tok
} // namespace cawk
