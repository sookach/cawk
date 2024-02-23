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

using vector = std::vector<std::variant<int64_t, double, std::string>>;

using cawk_val = std::variant<int64_t, double, std::string, vector>;

inline constexpr bool is_int(const cawk_val &c) noexcept {
  return std::holds_alternative<int64_t>(c);
}

inline constexpr bool is_double(const cawk_val &c) noexcept {
  return std::holds_alternative<double>(c);
}

inline constexpr bool is_string(const cawk_val &c) noexcept {
  return std::holds_alternative<std::string>(c);
}

inline constexpr bool is_vector(const cawk_val &x) noexcept {
  return std::holds_alternative<vector>(x);
}

inline constexpr std::optional<int64_t> to_int(const cawk_val &c) noexcept {
  if (is_int(c))
    return std::get<int64_t>(c);
  if (is_double(c))
    return static_cast<int64_t>(std::get<double>(c));
  if (is_string(c)) {
    try {
      return static_cast<int64_t>(std::stoll(std::get<std::string>(c)));
    } catch (...) {
    }
  }
  return std::nullopt;
}

inline constexpr std::optional<double> to_double(const cawk_val &c) noexcept {
  if (is_int(c))
    return static_cast<double>(std::get<int64_t>(c));
  if (is_double(c))
    return std::get<double>(c);
  if (is_string(c)) {
    try {
      return std::stod(std::get<std::string>(c));
    } catch (...) {
    }
  }
  return std::nullopt;
}

inline constexpr std::optional<std::string>
to_string(const cawk_val &x) noexcept {
  if (is_int(x))
    return std::to_string(std::get<int64_t>(x));
  if (is_double(x))
    return std::to_string(std::get<double>(x));
  if (is_string(x))
    return std::get<std::string>(x);
  return std::nullopt;
}

inline constexpr vector to_vector(const cawk_val &x) noexcept {
  return is_vector(x)   ? std::get<vector>(x)
         : is_int(x)    ? vector{std::get<int64_t>(x)}
         : is_double(x) ? vector{std::get<double>(x)}
         : is_string(x) ? vector{std::get<std::string>(x)}
                        : vector{};
}

bool operator==(const cawk_val &c, const std::integral auto &v) noexcept {
  return to_int(c) == v;
}

cawk_val operator+(const cawk_val &x, const cawk_val &y) noexcept {
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
    vector w{std::get<vector>(x)};
    std::ranges::move(z, std::back_inserter(w));
    return w;
  }
  return {};
}

int64_t operator+(const cawk_val &x, const std::integral auto &y) noexcept {
  if (auto z{to_int(x)}; z)
    return *z + y;
  std::cerr << "invalid operands for integer addition" << std::endl;
  exit(EXIT_FAILURE);
}

int64_t operator-(const cawk_val &x, const std::integral auto &y) noexcept {
  if (auto z{to_int(x)}; z)
    return *z + y;
  std::cerr << "invalid operands for integer subtraction" << std::endl;
  exit(EXIT_FAILURE);
}

std::vector<std::string> cawk_fields{};
std::string cawk_record{};

uint64_t NR{}, NF{};
bool BEGIN{true}, END{};
