#pragma once

#include "Exec/Value.h"

#include <string>
#include <vector>

namespace cawk {
Value split(Value String, Value &Array, Value FieldSep);
std::vector<std::string> split(std::string String, std::string Delim);
}