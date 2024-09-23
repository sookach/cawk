#pragma once

#include <cstdlib>
#include <functional>
#include <optional>
#include <string_view>

namespace cawk {

class Value;

template <typename T1, typename T2>
  requires requires(T1 X) { static_cast<T2 *>(&X); }
constexpr bool isa(const T2 *X) {
  return T1::classof(X);
}

template <typename T> constexpr T *ptr_cast(auto *X) {
  return static_cast<T *>(X);
}

template <typename T> constexpr const T *ptr_cast(const auto *X) {
  return static_cast<const T *>(X);
}

template <typename T> constexpr T *dyn_cast(auto *X) {
  return T::classof(X) ? static_cast<T *>(X) : nullptr;
}

template <typename T> constexpr const T *dyn_cast(const auto *X) {
  return T::classof(X) ? static_cast<const T *>(X) : nullptr;
}

template <typename T> constexpr T *dyn_cast_or_null(auto *X) {
  return X != nullptr && T::classof(X) ? static_cast<T *>(X) : nullptr;
}

template <typename T> constexpr const T *dyn_cast_or_null(const auto *X) {
  return X != nullptr && T::classof(X) ? static_cast<const T *>(X) : nullptr;
}

template <typename T> struct recursive final {
  const T F;

  constexpr recursive(T &&F) : F(std::forward<T>(F)) {}

  template <typename... Args>
  constexpr decltype(auto) operator()(Args &&...A) const noexcept {
    return F(std::cref(*this), std::forward<Args>(A)...);
  }
};

void cawk_unreachable(std::string_view S);

} // namespace cawk
