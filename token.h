#pragma once

#include "token_type.h"

#include <cstdint>
#include <string>

namespace cawk {
struct token final {
  std::string lexeme_{};
  token_type type_{};
  uint16_t line_{};
};
} // namespace cawk