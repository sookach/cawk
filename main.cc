#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <optional>
#include <ranges>
#include <regex>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using i128 = __int128_t;

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using u128 = __uint128_t;

using f32 = float;
using f64 = double;

using std::string;

template <typename T__> using slice = std::vector<T__>;

template <typename T__>
[[nodiscard]] inline bool match__(T__ &&x__, T__ &&y__) noexcept {
  return std::regex_match(std::regex{to_string(y__)}, to_string(x__));
}

template <typename T__>
[[nodiscard]] inline bool match__(T__ &&x__, std::string_view y__) noexcept {
  return std::regex_match(std::regex{y__.data()}, to_string(x__));
}

inline static constexpr struct {
  template <typename T1__, typename T2__>
  [[nodiscard]] constexpr T1__ operator()(T2__ &&x__) const noexcept {
    return static_cast<T1__>(x__);
  }

  template <typename T1__, typename T2__>
    requires(std::is_same_v<T1__, i8> || std::is_same_v<T1__, i16> ||
             std::is_same_v<T1__, i32>) &&
            std::is_same_v<std::remove_cvref_t<T2__>, string>
  [[nodiscard]] constexpr T1__ operator()(T2__ &&x__) const noexcept {
    try {
      return std::stoi(x__);
    } catch (...) {
      exit(EXIT_FAILURE);
    }
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, i64> &&
             std::is_same_v<std::remove_cvref_t<T2__>, string>
  [[nodiscard]] constexpr T1__ operator()(T2__ &&x__) const noexcept {
    try {
      return std::stoll(x__);
    } catch (...) {
      exit(EXIT_FAILURE);
    }
  }

  template <typename T1__, typename T2__>
    requires(std::is_same_v<T1__, u8> || std::is_same_v<T1__, u16> ||
             std::is_same_v<T1__, u32>) &&
            std::is_same_v<std::remove_cvref_t<T2__>, string>
  [[nodiscard]] constexpr T1__ operator()(T2__ &&x__) const noexcept {
    try {
      return std::stoul(x__);
    } catch (...) {
      exit(EXIT_FAILURE);
    }
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, u64> &&
             std::is_same_v<std::remove_cvref_t<T2__>, string>
  [[nodiscard]] constexpr T1__ operator()(T2__ &&x__) const noexcept {
    try {
      return std::stoull(x__);
    } catch (...) {
      exit(EXIT_FAILURE);
    }
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, f32> &&
             std::is_same_v<std::remove_cvref_t<T2__>, string>
  [[nodiscard]] constexpr T1__ operator()(T2__ &&x__) const noexcept {
    try {
      return std::stof(x__);
    } catch (...) {
      exit(EXIT_FAILURE);
    }
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, f64> &&
             std::is_same_v<std::remove_cvref_t<T2__>, string>
  [[nodiscard]] constexpr T1__ operator()(T2__ &&x__) const noexcept {
    try {
      return std::stod(x__);
    } catch (...) {
      exit(EXIT_FAILURE);
    }
  }
} cast__{};

std::vector<std::string> fields__{};
std::string record__{};

uint64_t NR{}, NF{};
bool BEGIN{true}, END{}, mid__{false};

f32 i = 0;

struct {
  void operator()() const {}
} run_begin__{};

struct {
  void operator()() const {
    { i += cast__.operator()<f32>(fields__[1]); }
  }
} run_mid__{};

struct {
  void operator()() const {
    { std::cout << i << NR << std::endl; }
  }
} run_end__{};

struct {
  bool operator()(std::istream &is__) const {
    fields__.clear();
    NF = 0;
    fields__.emplace_back();
    if (!std::getline(is__, fields__.back()))
      return false;
    std::stringstream ss__{fields__.back()};
    for (std::string s__; ss__ >> s__; ++NF)
      fields__.push_back(std::move(s__));
    ++NR;
    return true;
  }
} read_line__{};

struct {
  void operator()(std::istream &is__) const {
    run_begin__();
    for (; read_line__(is__);)
      run_mid__();
    run_end__();
  }
} run__{};

int main(int argc, char **argv) {
  if (argc == 2) {
    std::ifstream in__{argv[1]};
    run__(in__);
  } else {
    run_end__();
    run_begin__();
  }
}
