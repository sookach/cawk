//===- token.h - cawk token types -----------------------------------------===//
//
//  This file defines the token interface.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "token_type.h"

#include <cstdint>
#include <string>

namespace cawk {

/// @brief Token - Simple interface for a token. A token consists of three
/// parts: the raw source substring (lexeme), the type, and it's location in the
/// source file.
struct token final {
  /// @brief Token's lexeme.
  std::string lexeme_{};

  /// @brief Token's type.
  token_type type_{};

  /// @brief Token's line. (mostly for debugging purposes)
  uint16_t line_{};
};
} // namespace cawk