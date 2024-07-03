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

} // namespace cawk
