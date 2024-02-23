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

using vector_t__ = std::vector<std::variant<int64_t, double, std::string>>;

using val_t__ = std::variant<int64_t, double, std::string, vector_t__>;

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
  template <typename T__>
    requires std::is_same_v<T__, int64_t>
  [[nodiscard]] constexpr bool
  operator()(const std::string &s__) const noexcept {
    try {
      std::stoll(s);
      return true;
    } catch (...) {
      return false;
    }
  }

  template <typename T__>
    requires std::is_same_v<T__, double>
  [[nodiscard]] constexpr bool
  operator()(const std::string &s__) const noexcept {
    try {
      std::stod(s);
      return true;
    } catch (...) {
      return false;
    }
  }

} is_string_conv{};

inline static struct {
  template <typename T__>
  [[nodiscard]] constexpr bool operator()(const auto &x) const noexcept {
    if constexpr (std::is_same_v<T__, int64_t>)
      return is_int(x) || is_double(x) ||
             is_string(x) && is_string_conv<int>(x);

    if constexpr (std::is_same_v<T__, double>)
      return is_int(x) || is_double(x) ||
             is_string(x) && is_string_conv<double>(x);

    if constexpr (std::is_same_v<T__, std::string>)
      return !is_vector(x);

    return true;
  }
} is_conv{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x) const noexcept {
    return is_conv.operator()<int64_t>(x);
  }
} is_conv_int{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x) const noexcept {
    return is_conv.operator()<double>(x);
  }
} is_conv_double{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x) const noexcept {
    return is_conv.operator()<std::string>(x);
  }
} is_conv_string{};

inline static struct {
  [[nodiscard]] constexpr bool operator()(const auto &x) const noexcept {
    return is_conv.operator()<vector_t__>(x);
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

bool operator==(const val_t__ &x__, const std::integral auto &y__) noexcept {
  return to_int(x__) == y__;
}

/// Integer Operators.

template <typename T__>
  requires std::is_same_v<T__, val_t__>
[[nodiscard]] inline static constexpr int64_t
operator+(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) + y;
}

template <typename T__>
  requires std::is_same_v<T__, val_t__>
[[nodiscard]] inline static constexpr int64_t
operator-(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) - y;
}

template <typename T__>
  requires std::is_same_v<T__, val_t__>
[[nodiscard]] inline static constexpr int64_t
operator*(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) * y;
}

template <typename T__>
  requires std::is_same_v<T__, val_t__>
[[nodiscard]] inline static constexpr int64_t
operator/(T__ &&x__, std::integral auto &&y) noexcept {
  return to_int(x__) / y;
}

val_t__ operator+(const val_t__ &x, const val_t__ &y) noexcept {
  if (is_int(x)) {
    return x + to_int(y);
  } else if (is_double(x)) {
    x + to_double(y);
  } else if (is_string(x)) {
    if (is_conv_int(x)) {
      if (is_conv_int(y))
        return to_int(x) + to_int(y);
      if (is_conv_double(y))
        return to_int(x) + to_double(y);
      return to_string(x) + to_string(y);
    }
    if (is_conv_double(x)) {
      if (is_conv_int(y))
        return static_cast<int64_t>(to_double(y)) + to_int(y);
      if (is_conv_double(y))
        return to_double(x) + to_double(y);
    }
  } else if (is_vector(x)) {
    auto z{to_vector(y)};
    vector_t__ w{std::get<vector_t__>(x)};
    std::ranges::move(z, std::back_inserter(w));
    return w;
  }
  return {};
}

std::ostream &
operator<<(std::ostream &os,
           const std::variant<int64_t, double, std::string> &cawk_x) {
  if (is_int(cawk_x))
    return os << std::get<int64_t>(cawk_x);
  if (is_double(cawk_x))
    return os << std::get<double>(cawk_x);
  return os << std::get<std::string>(cawk_x);
}

std::ostream &operator<<(std::ostream &os, const val_t__ &cawk_x) {
  if (is_int(cawk_x))
    return os << std::get<int64_t>(cawk_x);
  if (is_double(cawk_x))
    return os << std::get<double>(cawk_x);
  if (is_string(cawk_x))
    return os << std::get<std::string>(cawk_x);

  os << "[ ";
  for (auto &&cawk_y : std::get<vector_t__>(cawk_x))
    os << cawk_y << ' ';
  return os << "]";
}

std::vector<std::string> fields__{};
std::string record__{};

uint64_t NR{}, NF{};
bool BEGIN{true}, END{};
