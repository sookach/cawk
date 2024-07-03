#pragma once

#include <cstdlib>
#include <optional>
#include <string_view>
namespace cawk {

class Value;

template <typename T> constexpr bool isa(const auto *X) {
  return T::classof(X);
}

template <typename T> constexpr T *ptr_cast(auto *X) {
  return static_cast<T *>(X);
}

template <typename T> constexpr const T *ptr_cast(const auto *X) {
  return static_cast<const T *>(X);
}

constexpr std::optional<double> ToFloat(std::string_view S) {
  char *Ptr;
  auto Value = std::strtod(S.data(), &Ptr);
  return Ptr == std::cend(S) ? std::make_optional(Value) : std::nullopt;
}

} // namespace cawk
