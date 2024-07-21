#pragma once

#include "Exec/Value.h"

#include <string>
#include <vector>

namespace cawk {
  std::string sprintf(std::string FormatString, std::vector<Value> Args);
}