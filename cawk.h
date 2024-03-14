#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <execution>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <ranges>
#include <regex>
#include <set>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) ||      \
    defined(__NetBSD__) || defined(__linux__)
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

namespace cawk {

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

using std::map;
using std::set;

template <typename T__> using hset = std::unordered_set<T__>;

template <typename Key__, typename T__>
using hmap = std::unordered_map<Key__, T__>;

template <typename T__> struct triple final {
  T__ first{}, second{}, third{};
};

template <typename T__>
concept regex__ =
    requires(T__ t__) { requires std::is_convertible_v<T__, string>; };

uint64_t NR_{}, NF_{};
char FS_{'1'}, RS_{'\n'};
bool BEGIN{true}, END{}, mid__{false};
std::string_view FILENAME_{};
std::vector<std::span<char>> fields__{};

std::unordered_map<std::string, triple<char *>> file_table__{}, cmd_table__{};

enum struct input_type__ { main__, file__, cmd__ };
triple<char *> bytes__{};
off_t size__{};

inline static constexpr struct {
  template <input_type__ T__ = input_type__::main__>
  inline constexpr void operator()(std::string_view name__) const noexcept {
    if constexpr (T__ == input_type__::main__)
      FILENAME_ = name__;

    if constexpr (T__ == input_type__::main__ || T__ == input_type__::file__) {
      const auto fd__{open(name__.data(), O_RDONLY)};
      if (fd__ == -1) [[unlikely]] {
        perror("");
        exit(errno);
      }

      auto [first__, last__, curr__]{[&]() noexcept -> triple<char *&> {
        if constexpr (T__ == input_type__::main__)
          return {bytes__.first, bytes__.second, bytes__.third};
        if constexpr (T__ == input_type__::file__)
          return {file_table__[name__.data()].first,
                  file_table__[name__.data()].second,
                  file_table__[name__.data()].third};
      }()};

      const auto size__{[fd__]() constexpr noexcept -> off_t {
        if (struct stat fileinfo__{}; fstat(fd__, &fileinfo__) != -1) [[likely]]
          return fileinfo__.st_size;
        perror("");
        exit(errno);
      }()};

      if ((first__ = curr__ = (char *)mmap(NULL, size__, PROT_READ, MAP_SHARED,
                                           fd__, 0)) == MAP_FAILED)
          [[unlikely]] {
        perror("");
        exit(errno);
      }

      close(fd__);

      last__ = first__ + size__;
    }

    if constexpr (T__ == input_type__::cmd__) {
      const auto pipe__{popen(name__.data(), "r")};
      if (pipe__ == nullptr) [[unlikely]] {
        perror("");
        exit(errno);
      }

      auto &[first__, last__, curr__]{cmd_table__[name__.data()]};

      static constexpr size_t buffer_size__{8192};
      first__ = curr__ = (char *)malloc(buffer_size__);

      const auto size__{fread(first__, sizeof(char), buffer_size__, pipe__)};
      if (ferror(pipe__)) [[unlikely]] {
        perror("");
        exit(errno);
      }

      pclose(pipe__);

      first__ = curr__ = (char *)realloc(first__, size__);
      last__ = first__ + size__;
    }
  }
} open__{};

inline static constexpr struct {
  template <input_type__ T__ = input_type__::main__>
  inline constexpr void
  operator()(std::string_view name__ = "") const noexcept {
    auto [first__,
          last__]{[&]() constexpr noexcept -> std::pair<char *, char *> {
      if constexpr (T__ == input_type__::main__)
        return {bytes__.first, bytes__.second};
      if constexpr (T__ == input_type__::file__)
        return {file_table__[name__.data()].first,
                file_table__[name__.data()].second};
      if constexpr (T__ == input_type__::cmd__)
        return {cmd_table__[name__.data()].first,
                cmd_table__[name__.data()].second};
    }()};

    if constexpr (T__ == input_type__::main__ || T__ == input_type__::file__) {
      if (munmap(first__, last__ - first__) == -1) [[unlikely]] {
        perror("");
        exit(EXIT_FAILURE);
      }
    }

    if constexpr (T__ == input_type__::cmd__)
      free(first__);

    if constexpr (T__ == input_type__::file__)
      file_table__.erase(name__.data());

    if constexpr (T__ == input_type__::cmd__)
      cmd_table__.erase(name__.data());
  }
} close__{};

inline static constexpr struct {
  template <input_type__ T1__ = input_type__::main__, bool T2__ = true>
  [[nodiscard]] inline constexpr bool
  operator()(std::string_view name__ = "",
             std::string *var__ = nullptr) const noexcept {
    auto [first__, last__]{[&]() -> std::pair<char *&, char *&> {
      if constexpr (T1__ == input_type__::main__)
        return {bytes__.third, bytes__.second};
      if constexpr (T1__ == input_type__::file__)
        return {file_table__[name__.data()].third,
                file_table__[name__.data()].second};
      if constexpr (T1__ == input_type__::cmd__)
        return {cmd_table__[name__.data()].third,
                cmd_table__[name__.data()].second};
    }()};

    return this->operator()<T2__>(first__, last__, var__);
  }

  template <bool T__ = true>
  [[nodiscard]] inline constexpr bool
  operator()(char *&first__, char *last__,
             std::string *var__ = nullptr) const noexcept {
    if constexpr (std::bool_constant<T__>::value) {
      NF_ = 0;
      fields__.clear();
    }

    if (first__ >= last__) [[unlikely]]
      return false;

    if constexpr (std::bool_constant<T__>::value)
      ++NR_;

    char *const start__{first__};

    if constexpr (std::bool_constant<T__>::value)
      fields__.emplace_back();

    for (char *prev__{}; first__ != last__;) {
      prev__ = std::find_if(first__, last__,
                            [](auto &&x__) constexpr noexcept -> bool {
                              return x__ == RS_ || x__ != FS_;
                            });

      if (*prev__ == '\n') [[unlikely]] {
        first__ = prev__;
        break;
      }

      first__ = std::find_if(prev__, last__,
                             [](auto &&x__) constexpr noexcept -> bool {
                               return x__ == RS_ || x__ == FS_;
                             });

      if constexpr (std::bool_constant<T__>::value) {
        fields__.emplace_back(prev__, first__);
        ++NF_;
      }
    }

    if constexpr (std::bool_constant<T__>::value)
      fields__.front() = {start__, first__};
    else
      *var__ = {start__, first__};

    ++first__;
    return true;
  }
} read_line__{};

inline std::function<void(void)> run_begin__{};
inline std::function<void(void)> run_mid__{};
inline std::function<void(void)> run_end__{};

inline static constexpr struct {
  void operator()() const noexcept {
    run_begin__();
    for (; read_line__();)
      run_mid__();
    run_end__();
  }
} run__{};

inline void init__() noexcept;

inline static constexpr struct {
  [[nodiscard]] __attribute__((pure)) inline constexpr bool
  operator()(regex__ auto &&x__, regex__ auto &&y__) const noexcept {
    return std::regex_search(x__, std::regex{y__});
  }

  [[nodiscard]] __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__, regex__ auto &&y__) const noexcept -> std::enable_if_t<
      std::is_same_v<std::remove_cvref_t<decltype(x__)>, std::span<char>>,
      bool> {
    return std::regex_search(string{std::cbegin(x__), std::cend(x__)},
                             std::regex{y__});
  }
} match__{};

inline static constexpr struct {
  template <typename T1__, typename T2__>
  [[nodiscard]] __attribute__((const)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    return static_cast<T1__>(x__);
  }

  template <typename T1__, typename T2__>
    requires(std::is_same_v<T1__, i8> || std::is_same_v<T1__, i16> ||
             std::is_same_v<T1__, i32>) &&
            (std::is_same_v<std::remove_cvref_t<T2__>, string> ||
             std::is_same_v<std::remove_cvref_t<T2__>, std::span<char>>)
  [[nodiscard]] __attribute__((pure)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    const auto y__{strtol(x__.data(), nullptr, 10)};
    if (errno == EINVAL || errno == ERANGE) [[unlikely]] {
      perror("");
      exit(errno);
    }

    return y__;
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, i64> &&
             (std::is_same_v<std::remove_cvref_t<T2__>, string> ||
              std::is_same_v<std::remove_cvref_t<T2__>, std::span<char>>)
  [[nodiscard]] __attribute__((pure)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    const auto y__{strtoll(x__.data(), nullptr, 10)};
    if (errno == EINVAL || errno == ERANGE) [[unlikely]] {
      perror("");
      exit(errno);
    }

    return y__;
  }

  template <typename T1__, typename T2__>
    requires(std::is_same_v<T1__, u8> || std::is_same_v<T1__, u16> ||
             std::is_same_v<T1__, u32>) &&
            (std::is_same_v<std::remove_cvref_t<T2__>, string> ||
             std::is_same_v<std::remove_cvref_t<T2__>, std::span<char>>)
  [[nodiscard]] __attribute__((pure)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    const auto y__{strtoul(x__.data(), nullptr, 10)};
    if (errno == EINVAL || errno == ERANGE) [[unlikely]] {
      perror("");
      exit(errno);
    }

    return y__;
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, u64> &&
             (std::is_same_v<std::remove_cvref_t<T2__>, string> ||
              std::is_same_v<std::remove_cvref_t<T2__>, std::span<char>>)
  [[nodiscard]] __attribute__((pure)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    const auto y__{strtoull(x__.data(), nullptr, 10)};
    if (errno == EINVAL || errno == ERANGE) [[unlikely]] {
      perror("");
      exit(errno);
    }

    return y__;
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, f32> &&
             (std::is_same_v<std::remove_cvref_t<T2__>, string> ||
              std::is_same_v<std::remove_cvref_t<T2__>, std::span<char>>)
  [[nodiscard]] __attribute__((pure)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    const auto y__{strtof(x__.data(), nullptr, 10)};
    if (errno == EINVAL || errno == ERANGE) [[unlikely]] {
      perror("");
      exit(errno);
    }

    return y__;
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, f64> &&
             (std::is_same_v<std::remove_cvref_t<T2__>, string> ||
              std::is_same_v<std::remove_cvref_t<T2__>, std::span<char>>)
  [[nodiscard]] __attribute__((pure)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    const auto y__{strtof(x__.data(), nullptr, 10)};
    if (errno == EINVAL || errno == ERANGE) [[unlikely]] {
      perror("");
      exit(errno);
    }

    return y__;
  }

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, f64> &&
             std::is_same_v<std::remove_cvref_t<T2__>, std::span<char>>
  [[nodiscard]] __attribute__((pure)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    const auto y__{strtod(x__.data(), nullptr, 10)};
    if (errno == EINVAL || errno == ERANGE) [[unlikely]] {
      perror("");
      exit(errno);
    }

    return y__;
  }
} cast__{};

inline constexpr struct {
  template <typename T__> constexpr void operator()(T__ &&x__) const noexcept {
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, i8>)
      printf("%hh\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, i16>)
      printf("%h\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, i32>)
      printf("%d\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, i64>)
      printf("%ll\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, u8>)
      printf("%uhh\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, u16>)
      printf("%uh\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, u32>)
      printf("%u\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, u64>)
      printf("%ull\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, f32>)
      printf("%f\n", x__);
    if constexpr (std::is_same_v<std::remove_cvref_t<T__>, f64>)
      printf("%e\n", x__);
  }
} print__{};

template <typename T__>
[[nodiscard]] auto operator+=(slice<T__> &s__, auto &&v__) noexcept
    -> std::enable_if_t<std::is_same_v<std::remove_cvref_t<decltype(v__)>, T__>,
                        slice<T__> &> {
  s__.push_back(std::forward<decltype(v__)>(v__));
  return s__;
}

template <typename T__>
[[nodiscard]] auto operator+=(slice<T__> &&s__, auto &&v__) noexcept
    -> std::enable_if_t<std::is_same_v<std::remove_cvref_t<decltype(v__)>, T__>,
                        slice<T__> &&> {
  s__.push_back(std::forward<decltype(v__)>(v__));
  return std::move(s__);
}

template <typename T__>
[[nodiscard]] auto operator+(slice<T__> &s__, auto &&v__) noexcept
    -> std::enable_if_t<std::is_same_v<std::remove_cvref_t<decltype(v__)>, T__>,
                        slice<T__> &> {
  return s__ + std::forward<decltype(v__)>(v__);
}

template <typename T__>
[[nodiscard]] auto operator+(slice<T__> &&s__, auto &&v__) noexcept
    -> std::enable_if_t<std::is_same_v<std::remove_cvref_t<decltype(v__)>, T__>,
                        slice<T__> &&> {
  return std::move(s__) + std::forward<decltype(v__)>(v__);
}

template <typename T__>
[[nodiscard]] std::ostream &operator<<(std::ostream &os__,
                                       const std::span<T__> &s__) {
  for (const auto &x__ : s__)
    os__ << x__;

  return os__;
}

template <typename T__, typename U__>
std::ostream &operator<<(std::ostream &os, const std::pair<T__, U__> &p) {
  return os << '[' << p.first << ' ' << p.second << ']';
}

template <typename T__>
auto operator+=(set<T__> &s__, auto &&v__) noexcept
    -> std::enable_if_t<std::is_same_v<std::remove_cvref_t<decltype(v__)>, T__>,
                        set<T__> &> {
  s__.insert(std::forward<decltype(v__)>(v__));
  return s__;
}

template <typename T__>
auto operator+=(set<T__> &&s__, auto &&v__) noexcept
    -> std::enable_if_t<std::is_same_v<std::remove_cvref_t<decltype(v__)>, T__>,
                        set<T__> &&> {
  s__.insert(std::forward<decltype(v__)>(v__));
  return std::move(s__);
}

[[nodiscard]] __attribute__((const)) constexpr bool
operator==(std::span<char> s1__, std::string_view s2__) noexcept {
  return std::ranges::equal(s1__, s2__);
}

[[nodiscard]] __attribute__((const)) constexpr bool
operator==(std::span<char> s1__, std::span<char> s2__) noexcept {
  return std::ranges::equal(s1__, s2__);
}

using std::cos;
using std::exp;
using std::log;
using std::rand;
using std::sin;
using std::sqrt;
using std::srand;
using std::system;

inline static constexpr struct {
  [[nodiscard]] __attribute__((const)) inline constexpr std::string::size_type
  operator()(auto &&in__, auto &&find__) const
    requires(std::is_same_v<std::remove_cvref_t<decltype(in__)>, string> ||
             std::is_same_v<std::remove_cvref_t<decltype(in__)>,
                            std::string_view>) &&
            (std::is_same_v<std::remove_cvref_t<decltype(find__)>, string> ||
             std::is_same_v<std::remove_cvref_t<decltype(find__)>,
                            std::string_view>)
  {
    return std::cbegin(std::execution::par_unseq,
                       std::search(std::cbegin(in__), std::cbegin(find__))) -
           std::cbegin(in__);
  }

  [[nodiscard]] __attribute__((const)) inline constexpr std::string::size_type
  operator()(auto &&in__, auto &&find__) const
    requires(std::is_same_v<std::remove_cvref_t<decltype(in__)>, string> ||
             std::is_same_v<std::remove_cvref_t<decltype(in__)>,
                            std::string_view>) &&
            std::is_same_v<std::remove_cvref_t<decltype(find__)>, char>
  {
    return std::cbegin(std::find(std::execution::par_unseq, in__, find__)) -
           std::cbegin(in__);
  }
} index_{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((const)) inline constexpr auto
  operator()(auto &&c__) const noexcept {
    return std::size(c__);
  }
} length_{};

inline static constexpr struct {
  __attribute__((const)) inline constexpr void
  operator()(auto &&string__, auto &&array__, auto &&fieldsep__) const noexcept
    requires std::__is_same_uncvref<decltype(string__), string>::value
             && std::__is_same_uncvref<decltype(array__), slice<string>>::value
             && std::__is_same_uncvref<decltype(fieldsep__), char>::value
  {
    auto first__{std::cbegin(string__)}, next__{std::cbegin(string__)};
    for (; first__ != std::cend(string__);) {
      first__ =
          std::find_if(std::execution::par_unseq, first__, std::cend(string__),
                       [fieldsep__](auto &&x__) constexpr noexcept -> bool {
                         return x__ != fieldsep__;
                       });

      if (first__ == std::cend(string__)) [[unlikely]]
        break;

      next__ = std::find(std::execution::par_unseq, first__,
                         std::cend(string__), fieldsep__);
      array__.emplace_back(first__, next__);
      first__ = next__;
    }
  }
} split_{};

inline static constexpr struct {
  inline constexpr void operator()(auto &&r__) const noexcept {
    std::sort(std::execution::par_unseq, std::begin(r__), std::end(r__));
  }
} sort_{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((const)) inline constexpr auto
  operator()(auto &&r__) const noexcept {
    auto x__{std::forward<decltype(r__)>(r__)};
    std::sort(std::execution::par_unseq, std::begin(x__), std::end(x__));
    return x__;
  }
} asort_{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((const)) inline u64 operator()() const noexcept {
    return static_cast<u64>(
        std::chrono::duration_cast<std::chrono::seconds>(
            std::chrono::system_clock::now().time_since_epoch())
            .count());
  }
} systime_{};

inline static constexpr struct {
  [[nodiscard]] inline constexpr bool operator()() const {
    return read_line__();
  }

  [[nodiscard]] inline constexpr bool
  operator()(std::string &v__) const noexcept {
    return read_line__.operator()<input_type__::main__, false>("", &v__);
  }

  template <bool T__ = true>
  [[nodiscard]] inline constexpr bool
  operator()(std::string_view f__) const noexcept {
    if constexpr (std::bool_constant<T__>::value) {
      const auto path__{[f__]() noexcept -> std::string {
        return (!std::empty(f__) && f__.front() == '/'
                    ? ""
                    : std::string{std::filesystem::current_path()} + '/') +
               std::string{f__};
      }()};

      if (!file_table__.contains(path__.data()))
        open__.operator()<input_type__::file__>(path__.data());

      return read_line__.operator()<input_type__::file__>(path__.data());
    }

    if constexpr (!std::bool_constant<T__>::value) {
      if (!cmd_table__.contains(f__.data()))
        open__.operator()<input_type__::cmd__>(f__.data());

      return read_line__.operator()<input_type__::cmd__>(f__.data());
    }
  }
} getline{};

enum struct json_token_type__ {
  l_square,
  r_square,
  l_brace,
  r_brace,
  colon,
  comma,
  string_literal,
  other_literal,
  eof
};

struct json_token__ final {
  json_token_type__ type__{};
  string lexeme__{};
};

inline static constexpr struct {
  constexpr slice<json_token__>
  operator()(std::string_view source__) const noexcept {
    auto prev__{std::cbegin(source__)}, next__{prev__};
    slice<json_token__> tokens__{};

    for (; next__ != std::cend(source__);) {
      switch (prev__ = next__++; *prev__) {
      default: {
        string value__{};
        for (; next__ != std::cend(source__) && std::isalpha(*next__); ++next__)
          value__.push_back(*next__);

        tokens__.emplace_back(json_token_type__::other_literal, value__);
        break;
      }
      case ' ':
        [[fallthrough]];
      case '\t':
        [[fallthrough]];
      case '\n':
        [[fallthrough]];
      case '\r':
        break;
      case '[':
        tokens__.emplace_back(json_token_type__::l_square);
        break;
      case ']':
        tokens__.emplace_back(json_token_type__::r_square);
        break;
      case '{':
        tokens__.emplace_back(json_token_type__::l_brace);
        break;
      case '}':
        tokens__.emplace_back(json_token_type__::r_brace);
        break;
      case ':':
        tokens__.emplace_back(json_token_type__::colon);
        break;
      case ',':
        tokens__.emplace_back(json_token_type__::comma);
        break;
      case '"': {
        string value__{};
        for (; next__ != std::cend(source__) && *next__ != '"'; ++next__)
          value__.push_back(*next__);

        if (next__ == std::cend(source__)) [[unlikely]] {
          fprintf(stderr, "unexpected eoi while parsing json");
          exit(EXIT_FAILURE);
        }

        ++next__;
        tokens__.emplace_back(json_token_type__::string_literal, value__);
        break;
      }
      case '0':
        [[fallthrough]];
      case '1':
        [[fallthrough]];
      case '2':
        [[fallthrough]];
      case '3':
        [[fallthrough]];
      case '4':
        [[fallthrough]];
      case '5':
        [[fallthrough]];
      case '6':
        [[fallthrough]];
      case '7':
        [[fallthrough]];
      case '8':
        [[fallthrough]];
      case '9':
        [[fallthrough]];
      case '-': {
        string value__{*prev__};
        for (; next__ != std::cend(source__) && std::isdigit(*next__); ++next__)
          value__.push_back(*next__);

        if (next__ < std::cend(source__) - 1 && *next__ == '.' &&
            std::isdigit(next__[1]))
          for (++next__; next__ != std::cend(source__) && std::isdigit(*next__);
               ++next__)
            value__.push_back(*next__);

        tokens__.emplace_back(json_token_type__::other_literal, value__);
      }
      }
    }

    tokens__.emplace_back(json_token_type__::eof);

    return tokens__;
  }
} lex_json__{};

inline static constexpr struct {
  hmap<string, string> operator()(std::string_view source__) const noexcept {
    const auto tokens__{lex_json__(source__)};
    hmap<string, string> values__{};

    auto next__{std::cbegin(tokens__)};

    auto match__{[&next__](json_token_type__ t__) constexpr noexcept -> bool {
      if (next__->type__ != t__)
        return false;
      ++next__;
      return true;
    }};

    auto expect__{[&match__](json_token_type__ t__) constexpr noexcept -> void {
      if (!match__(t__)) [[unlikely]] {
        fprintf(stderr, "json parsing error.\n");
        exit(EXIT_FAILURE);
      }
    }};

    auto expect_one_of__{
        [&match__](json_token_type__ t1__,
                   json_token_type__ t2__) constexpr noexcept -> void {
          if (!match__(t1__) && !match__(t2__)) [[unlikely]] {
            fprintf(stderr, "json parsing error.\n");
            exit(EXIT_FAILURE);
          }
        }};

    if (expect__(json_token_type__::l_brace);
        match__(json_token_type__::r_brace))
      return values__;

    expect__(json_token_type__::string_literal);
    expect__(json_token_type__::colon);
    expect_one_of__(json_token_type__::string_literal,
                    json_token_type__::other_literal);
    values__[(next__ - 3)->lexeme__] = (next__ - 1)->lexeme__;

    for (; match__(json_token_type__::comma);) {
      expect__(json_token_type__::string_literal);
      expect__(json_token_type__::colon);
      expect_one_of__(json_token_type__::string_literal,
                      json_token_type__::other_literal);
      values__[(next__ - 3)->lexeme__] = (next__ - 1)->lexeme__;
    }

    expect__(json_token_type__::r_brace);

    return values__;
  }

  hmap<string, string> operator()(std::span<char> source__) const noexcept {
    return this->operator()(std::string_view{source__});
  }
} parse_basic_json__{};

inline static constexpr struct {
  [[nodiscard]] inline constexpr i32
  operator()(std::string_view name__) const noexcept {
    if (file_table__.contains(name__.data()))
      close__.operator()<input_type__::file__>(name__);
    else
      close__.operator()<input_type__::cmd__>(name__);
    return 0;
  }
} close_{};

} // namespace cawk

int main(int argc, char **argv) {
  cawk::init__();
  if (argc == 2) {
    cawk::open__(argv[1]);
    cawk::run__();
    cawk::close__();
  } else {
    cawk::run_begin__();
  }
}