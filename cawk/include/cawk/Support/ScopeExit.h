//===- ScopeExit.h --------------------------------------------------------===//
// This file provides a simple implementation of the scope exit pattern.
//===----------------------------------------------------------------------===//

#pragma once

#include <type_traits>

namespace cawk {
namespace detail {
struct make_scope_exit_impl;

/// scope_exit - A simple implementation of the scope exit pattern.
template <typename T> class scope_exit {
  /// The function to call at the end of the scope.
  T Fn;

  /// A flag to determine if the scope exit is activated.
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

/// make_scope_exit - A factory function to create a scope_exit object.
struct make_scope_exit_impl {
  template <typename T> static auto operator()(T &&Fn) {
    return scope_exit(std::forward<T>(Fn));
  }
};
} // namespace detail

detail::make_scope_exit_impl make_scope_exit;
} // namespace cawk