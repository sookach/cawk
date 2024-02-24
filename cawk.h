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

std::vector<std::string> fields__{};
std::string record__{};

uint64_t NR{}, NF{};
bool BEGIN{true}, END{};
