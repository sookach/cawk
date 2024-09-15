#include "Support/Support.h"

#include <cassert>

using namespace cawk;

void cawk::cawk_unreachable(std::string_view S) { assert(0 && S.data()); }