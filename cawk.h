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
  constexpr bool operator()(const T__ &x__) const noexcept {
    return std::holds_alternative<int64_t>(x__);
  }
} is_int;

inline static struct {
  template <typename T__>
  constexpr bool operator()(const T__ &x__) const noexcept {
    return std::holds_alternative<double>(x__);
  }
} is_double{};

inline static struct {
  template <typename T__>
  constexpr bool operator()(const T__ &x__) const noexcept {
    return std::holds_alternative<std::string>(x__);
  }
} is_string{};

inline static struct {
  template <typename T__>
  constexpr bool operator()(const T__ &x__) const noexcept {
    return std::holds_alternative<vector_t__>(x__);
  }
} is_vector{};

inline static struct {
  constexpr int64_t operator()(const val_t__ &v__) const noexcept {
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

  constexpr int64_t operator()(val_t__ &&v__) const noexcept {
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
  constexpr double operator()(const val_t__ &v__) noexcept {
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

  constexpr double operator()(val_t__ &&v__) noexcept {
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

inline constexpr std::string to_string(const val_t__ &v__) noexcept {
  if (is_int(v__))
    return std::to_string(std::get<int64_t>(v__));
  if (is_double(v__))
    return std::to_string(std::get<double>(v__));
  if (is_string(v__))
    return std::get<std::string>(v__);

  std::cerr << "invalid string conversion." << std::endl;
  exit(EXIT_FAILURE);
}

inline constexpr std::string to_string(val_t__ &&v__) noexcept {
  if (is_int(v__))
    return std::to_string(std::get<int64_t>(v__));
  if (is_double(v__))
    return std::to_string(std::get<double>(v__));
  if (is_string(v__))
    return std::get<std::string>(v__);

  std::cerr << "invalid string conversion." << std::endl;
  exit(EXIT_FAILURE);
}

inline constexpr vector_t__ to_vector(const val_t__ &v__) noexcept {
  return is_vector(v__)   ? std::get<vector_t__>(v__)
         : is_int(v__)    ? vector_t__{std::get<int64_t>(v__)}
         : is_double(v__) ? vector_t__{std::get<double>(v__)}
         : is_string(v__) ? vector_t__{std::get<std::string>(v__)}
                          : vector_t__{};
}

bool operator==(const val_t__ &x, const std::integral auto &y) noexcept {
  return to_int(x) == y;
}

/// Integer Operators.

int64_t operator+(const val_t__ &x, const std::integral auto &y) noexcept {
  if (auto z{to_int(x)}; z)
    return *z + y;
  std::cerr << "invalid operands for integer addition" << std::endl;
  exit(EXIT_FAILURE);
}

int64_t operator-(const val_t__ &x, const std::integral auto &y) noexcept {
  if (auto z{to_int(x)}; z)
    return *z - y;
  std::cerr << "invalid operands for integer subtraction" << std::endl;
  exit(EXIT_FAILURE);
}

int64_t operator*(const val_t__ &x, const std::integral auto &y) noexcept {
  if (auto z{to_int(x)}; z)
    return *z * y;
  std::cerr << "invalid operands for integer addition" << std::endl;
  exit(EXIT_FAILURE);
}

int64_t operator/(const val_t__ &x, const std::integral auto &y) noexcept {
  if (auto z{to_int(x)}; z)
    return *z / y;
  std::cerr << "invalid operands for integer subtraction" << std::endl;
  exit(EXIT_FAILURE);
}

val_t__ operator+(const val_t__ &x, const val_t__ &y) noexcept {
  if (is_int(x)) {
    if (auto z{to_int(y)}; z)
      return x + *z;
  } else if (is_double(x)) {
    if (auto z{to_double(y)}; z)
      return x + *z;
  } else if (is_string(x)) {
    if (auto z{to_string(y)}; z)
      return x + *z;
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
