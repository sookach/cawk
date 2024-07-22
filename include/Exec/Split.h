#pragma once

#include "Exec/Value.h"

namespace cawk {
Value split(Value String, Value &Array, Value FieldSep);
}