#pragma once

#include <type_traits>

namespace cawk {
namespace detail {
struct make_scope_exit_impl;

template <typename T> class scope_exit {
  T Fn;
  bool Activated = true;

  friend struct make_scope_exit_impl;

  explicit scope_exit(T &&Fn) : Fn(std::forward<T>(Fn)) {}

public:
  ~scope_exit() {
    if (Activated)
      Fn();
  }

  void deactivate() { Activated = false; }
};

struct make_scope_exit_impl {
  template <typename T> static auto operator()(T &&Fn) {
    return scope_exit(std::forward<T>(Fn));
  }
};
} // namespace detail

detail::make_scope_exit_impl make_scope_exit;
} // namespace cawk