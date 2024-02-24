#include <algorithm>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <optional>
#include <ranges>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

using primitive_t__ = std::variant<int64_t, double, std::string>;
using vector_t__ = std::vector<primitive_t__>;
using val_t__ = std::variant<int64_t, double, std::string, vector_t__>;

template <typename T__>
concept numeric__ = std::is_integral_v<T__> || std::is_floating_point_v<T__>;

inline static struct {
  template <typename T__>
  [[nodiscard]] constexpr bool operator()(const auto &x__) const noexcept {
    return std::holds_alternative<T__>(x__);
  }
} is{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x__) const noexcept {
    return is.operator()<int64_t>(x__);
  }
} is_int;

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x__) const noexcept {
    return std::holds_alternative<double>(x__);
  }
} is_double{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x__) const noexcept {
    return std::holds_alternative<std::string>(x__);
  }
} is_string{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x__) const noexcept {
    return std::holds_alternative<vector_t__>(x__);
  }
} is_vector{};

inline static struct {
  template <numeric__ T__>
  [[nodiscard]] constexpr bool operator()(const val_t__ &x__) const noexcept {
    try {
      if constexpr (std::is_same_v<T__, int64_t>)
        std::stoll(std::get<std::string>(x__));
      else if constexpr (std::is_same_v<T__, double>)
        std::stod(std::get<std::string>(x__));
      return true;
    } catch (...) {
      return false;
    }
  }

  template <numeric__ T__>
  [[nodiscard]] constexpr bool operator()(val_t__ &&x__) const noexcept {
    try {
      if constexpr (std::is_same_v<T__, int64_t>)
        std::stoll(std::get<std::string>(std::move(x__)));
      else if constexpr (std::is_same_v<T__, double>)
        std::stod(std::get<std::string>(std::move(x__)));
      return true;
    } catch (...) {
      return false;
    }
  }

} is_string_conv{};

inline static struct {
  template <typename T__>
  [[nodiscard]] constexpr bool operator()(const val_t__ &x__) const noexcept {
    if constexpr (std::is_same_v<T__, int64_t> || std::is_same_v<T__, double>)
      return is_int(x__) || is_double(x__) ||
             is_string(x__) && is_string_conv.operator()<T__>(x__);

    if constexpr (std::is_same_v<T__, std::string>)
      return !is_vector(x__);

    return true;
  }
} is_conv{};

inline static struct {
  [[nodiscard]] constexpr auto operator()(auto &&x__) const noexcept
      -> std::enable_if_t<
          std::is_same_v<std::remove_cvref_t<decltype(x__)>, val_t__>, bool> {
    return is_conv.operator()<int64_t>(x__);
  }
} is_conv_int{};

inline static struct {
  [[nodiscard]] constexpr auto operator()(auto &&x__) const noexcept
      -> std::enable_if_t<
          std::is_same_v<std::remove_cvref_t<decltype(x__)>, val_t__>, bool> {
    return is_conv.operator()<double>(x__);
  }
} is_conv_double{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x) const noexcept {
    return is_conv.operator()<std::string>(x);
  }
} is_conv_string{};

inline static struct {
  [[nodiscard]] constexpr auto operator()(auto &&x__) const noexcept
      -> std::enable_if_t<
          std::is_same_v<std::remove_cvref_t<decltype(x__)>, val_t__>, bool> {
    return is_conv.operator()<vector_t__>(x__);
  }
} is_conv_vector{};

inline static struct {
  [[nodiscard]] constexpr int64_t
  operator()(const val_t__ &v__) const noexcept {
    if (is_int(v__))
      return std::get<int64_t>(v__);
    if (is_double(v__))
      return static_cast<int64_t>(std::get<double>(v__));
    if (is_string(v__)) {
      try {
        return static_cast<int64_t>(std::stoll(std::get<std::string>(v__)));
      } catch (...) {
      }
    }
    std::cerr << "invalid int conversion." << std::endl;
    exit(EXIT_FAILURE);
  }

  [[nodiscard]] constexpr int64_t operator()(val_t__ &&v__) const noexcept {
    if (is_int(v__))
      return std::get<int64_t>(std::move(v__));
    if (is_double(v__))
      return static_cast<int64_t>(std::get<double>(std::move(v__)));
    if (is_string(v__)) {
      try {
        return static_cast<int64_t>(
            std::stoll(std::get<std::string>(std::move(v__))));
      } catch (...) {
      }
    }
    std::cerr << "invalid int conversion." << std::endl;
    exit(EXIT_FAILURE);
  }
} to_int{};

inline static struct {
  [[nodiscard]] constexpr double operator()(const val_t__ &v__) const noexcept {
    if (is_int(v__))
      return static_cast<double>(std::get<int64_t>(v__));
    if (is_double(v__))
      return std::get<double>(v__);
    if (is_string(v__)) {
      try {
        return std::stod(std::get<std::string>(v__));
      } catch (...) {
      }
    }
    std::cerr << "invalid floating point conversion." << std::endl;
    exit(EXIT_FAILURE);
  }

  [[nodiscard]] constexpr double operator()(val_t__ &&v__) const noexcept {
    if (is_int(v__))
      return static_cast<double>(std::get<int64_t>(std::move(v__)));
    if (is_double(v__))
      return std::get<double>(std::move(v__));
    if (is_string(v__)) {
      try {
        return std::stod(std::get<std::string>(std::move(v__)));
      } catch (...) {
      }
    }
    std::cerr << "invalid floating point conversion." << std::endl;
    exit(EXIT_FAILURE);
  }
} to_double{};

inline static struct {
  [[nodiscard]] constexpr std::string
  operator()(const val_t__ &v__) const noexcept {
    if (is_int(v__))
      return std::to_string(std::get<int64_t>(v__));
    if (is_double(v__))
      return std::to_string(std::get<double>(v__));
    if (is_string(v__))
      return std::get<std::string>(v__);

    std::cerr << "invalid string conversion." << std::endl;
    exit(EXIT_FAILURE);
  }

  [[nodiscard]] constexpr std::string operator()(val_t__ &&v__) const noexcept {
    if (is_int(v__))
      return std::to_string(std::get<int64_t>(std::move(v__)));
    if (is_double(v__))
      return std::to_string(std::get<double>(std::move(v__)));
    if (is_string(v__))
      return std::get<std::string>(std::move(v__));

    std::cerr << "invalid string conversion." << std::endl;
    exit(EXIT_FAILURE);
  }
} to_string{};

inline static struct {
  [[nodiscard]] constexpr vector_t__
  operator()(const val_t__ &v__) const noexcept {
    return is_vector(v__)   ? std::get<vector_t__>(v__)
           : is_int(v__)    ? vector_t__{std::get<int64_t>(v__)}
           : is_double(v__) ? vector_t__{std::get<double>(v__)}
           : is_string(v__) ? vector_t__{std::get<std::string>(v__)}
                            : vector_t__{};
  }

  [[nodiscard]] constexpr vector_t__ operator()(val_t__ &&v__) const noexcept {
    return is_vector(v__)   ? std::get<vector_t__>(std::move(v__))
           : is_int(v__)    ? vector_t__{std::get<int64_t>(std::move(v__))}
           : is_double(v__) ? vector_t__{std::get<double>(std::move(v__))}
           : is_string(v__) ? vector_t__{std::get<std::string>(std::move(v__))}
                            : vector_t__{};
  }
} to_vector{};

/// integer operators.

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr bool
operator==(T__ &&x__, std::integral auto &&y__) noexcept {
  return to_int(x__) == y__;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator+(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) + y;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator-(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) - y;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator*(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) * y;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator/(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) / y;
}

/// floating-point operators.

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr bool
operator==(T__ &&x__, std::floating_point auto &&y__) noexcept {
  return to_double(x__) == y__;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator+(T__ &&x__, std::floating_point auto &&y) noexcept {
  return to_double(x__) + y;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator-(T__ &&x__, std::floating_point auto &&y) noexcept {
  return to_double(x__) - y;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator*(T__ &&x__, std::floating_point auto &&y) noexcept {
  return to_double(x__) * y;
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline static constexpr int64_t
operator/(T__ &&x__, std::floating_point auto &&y) noexcept {
  return to_double(x__) / y;
}

/// val to val operators

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline val_t__ operator+(T__ &&x__, T__ &&y__) noexcept {
  if (is_int(x__)) {
    return x__ + to_int(y__);
  } else if (is_double(x__)) {
    return x__ + to_double(y__);
  } else if (is_string(x__)) {
    if (is_conv_int(x__)) {
      if (is_conv_int(y__))
        return to_int(x__) + to_int(y__);
      if (is_conv_double(y__))
        return to_int(x__) + to_double(y__);
      return to_string(x__) + to_string(y__);
    }
    if (is_conv_double(x__)) {
      if (is_conv_int(y__))
        return static_cast<int64_t>(to_double(y__)) + to_int(y__);
      if (is_conv_double(y__))
        return to_double(x__) + to_double(y__);
    }
  } else if (is_vector(x__)) {
    auto z__{to_vector(y__)};
    vector_t__ w__{std::get<vector_t__>(x__)};
    std::ranges::move(z__, std::back_inserter(w__));
    return w__;
  }
  return {};
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline val_t__ operator-(T__ &&x__, T__ &&y__) noexcept {
  if (is_int(x__))
    return std::get<int64_t>(x__) - to_int(y__);
  if (is_double(x__)) {
    if (is_int(y__))
      return to_int(x__) - to_int(y__);
    return to_double(x__) - to_double(y__);
  }

  if (is_conv_double(x__)) {
    if (is_conv_int(y__))
      return to_int(x__) - to_int(y__);
    return to_double(x__) - to_double(y__);
  }

  if (is_conv_int(x__))
    return to_int(x__) - to_int(y__);

  return {};
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline val_t__ operator*(T__ &&x__, T__ &&y__) noexcept {
  if (is_int(x__))
    return std::get<int64_t>(x__) * to_int(y__);
  if (is_double(x__)) {
    if (is_int(y__))
      return to_int(x__) * to_int(y__);
    return to_double(x__) * to_double(y__);
  }

  if (is_conv_double(x__)) {
    if (is_conv_int(y__))
      return to_int(x__) * to_int(y__);
    return to_double(x__) * to_double(y__);
  }

  if (is_conv_int(x__))
    return to_int(x__) * to_int(y__);

  return {};
}

template <typename T__>
  requires requires(T__ t) { std::is_same_v<T__, val_t__>; }
[[nodiscard]] inline val_t__ operator/(T__ &&x__, T__ &y__) noexcept {
  if (is_int(x__))
    return std::get<int64_t>(x__) / to_int(y__);
  if (is_double(x__)) {
    if (is_int(y__))
      return to_int(x__) / to_int(y__);
    return to_double(x__) / to_double(y__);
  }

  if (is_conv_double(x__)) {
    if (is_conv_int(y__))
      return to_int(x__) / to_int(y__);
    return to_double(x__) / to_double(y__);
  }

  if (is_conv_int(x__))
    return to_int(x__) / to_int(y__);

  return {};
}

std::ostream &operator<<(std::ostream &os__, const primitive_t__ &x__) {
  if (is_int(x__))
    return os__ << std::get<int64_t>(x__);
  if (is_double(x__))
    return os__ << std::get<double>(x__);
  return os__ << std::get<std::string>(x__);
}

std::ostream &operator<<(std::ostream &os__, const val_t__ &x__) {
  if (is_int(x__))
    return os__ << std::get<int64_t>(x__);
  if (is_double(x__))
    return os__ << std::get<double>(x__);
  if (is_string(x__))
    return os__ << std::get<std::string>(x__);

  os__ << "[ ";
  for (auto &&y__ : std::get<vector_t__>(x__))
    os__ << y__ << ' ';
  return os__ << ']';
}

std::vector<std::string> fields__{};
std::string record__{};

uint64_t NR{}, NF{};
bool BEGIN{true}, END{};
