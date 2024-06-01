#pragma once

#include <string_view>

namespace cawk {
namespace tok {
enum TokenKind : unsigned short {
#define TOK(ID) ID,
#include "TokenKinds.def"
  NUM_TOKENS
};

std::string_view GetTokenName(TokenKind Kind);
std::string_view GetPunctuatorSpelling(TokenKind Kind);
std::string_view GetKeywordSpelling(TokenKind Kind);
} // namespace tok
} // namespace cawk
