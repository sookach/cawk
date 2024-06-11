#pragma once

#include <cstdlib>
#include <optional>
#include <string_view>
namespace cawk {

class Value;

template <typename T1, typename T2> constexpr bool isa(const T2 *X) {
  return T1::classof(X);
}

constexpr std::optional<double> ToFloat(std::string_view S) {
  char *Ptr;
  auto Value = std::strtod(S.data(), &Ptr);
  return Ptr == std::cend(S) ? std::make_optional(Value) : std::nullopt;
}

} // namespace cawk
