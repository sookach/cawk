#pragma once

#include "Basic/Diagnostic.h"

#include <string_view>

namespace cawk {
class Instance {
  Diagnostic Diags;
  std::string_view Source;

public:
  Instance(std::string_view Source) : Source(Source) {}

  int execute();
};
} // namespace cawk