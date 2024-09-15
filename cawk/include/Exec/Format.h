#pragma once

#include "Exec/Value.h"

#include <string>
#include <vector>

namespace cawk {
std::string format(std::string FormatString, std::vector<Value *> Args);

template <typename... T>
std::string format(std::string FormatString, T... Args);
} // namespace cawk