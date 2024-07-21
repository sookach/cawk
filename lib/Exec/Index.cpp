#include "Exec/Index.h"

using namespace cawk;

std::string::size_type cawk::index(std::string In, std::string Find) {
  auto I = In.find(Find);
  return I == std::string::npos ? 0 : I + 1;
}