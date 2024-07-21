#include "Exec/Sprintf.h"
#include "Exec/Format.h"

using namespace cawk;

std::string cawk::sprintf(std::string FormatString, std::vector<Value> Args) {
  return format(FormatString, Args);
}