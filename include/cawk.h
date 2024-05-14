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
#include <random>
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

/// Predefined Variables

u64 NR_{}, NF_{};
char FS_{' '}, RS_{'\n'};
std::string_view FILENAME_{};

i64 RSTART_{}, RLENGTH_{};

bool BEGIN{true}, END{}, mid__{false};
std::vector<std::span<char>> fields__{};

std::unordered_map<std::string, triple<char *>> file_table__{}, cmd_table__{};

enum struct input_type__ { main__, file__, cmd__ };
triple<char *> bytes__{};
off_t size__{};

auto positional__{[]() consteval -> std::array<i8, 5> {
  std::array<i8, 5> a{};
  std::ranges::fill(a, -1);
  return a;
}()};

inline static constexpr struct {
  inline constexpr void operator()(int argc, char **argv) const noexcept {
    int n{};

    for (int i{1};;) {
      if (i == argc)
        break;

      switch (argv[i][0]) {
      default:
        if (n == std::size(positional__)) [[unlikely]] {
          fprintf(stderr, "Too many command line arguments (max %zu)\n",
                  std::size(positional__));
          exit(EXIT_FAILURE);
        }
        positional__[n++] = i++;
        break;
      case '-':
        switch (argv[i][1]) {
        default:
          fprintf(stderr, "Unrecognized option: %s\n", argv[i]);
          exit(EXIT_FAILURE);
        case 'F': {
          auto fs_err{[&] -> void {
            fprintf(stderr, "Illegal FS: ");
            for (int j{2}; argv[i][j] != '\0'; ++j)
              fprintf(stderr, "%c", argv[i][j]);
            fputs("", stderr);
          }};

          switch (argv[i][2]) {
          default:
            if (argv[i][3] != '\0')
              fs_err();
            FS_ = argv[i][2];
            break;
          case '\0':
            fs_err();
          case '\'':
            if (argv[i][4] != '\'')
              fs_err();
            FS_ = argv[i][3];
          }

          ++i;
        }
        }
      }
    }
  }
} parse_cmd_line__{};

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

  template <typename T1__, typename T2__>
    requires std::is_same_v<T1__, string> && std::is_arithmetic_v<T2__>
  [[nodiscard]] __attribute__((const)) constexpr T1__
  operator()(T2__ &&x__) const noexcept {
    return std::to_string(x__);
  }
} cast__{};

inline constexpr struct {
  // __attribute__((__const__)) constexpr void operator()() const noexcept {}

  template <typename T__, typename... Args__>
  constexpr void operator()(T__ &&x__, Args__ &&...y__) const noexcept {
    if constexpr (sizeof...(y__) == 1)
      std::cout << x__;
    else {
      std::cout << x__ << ' ';
      this->operator()(y__...);
    }
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

inline static constexpr struct {
  template <typename T1, typename T2>
  [[nodiscard]] constexpr decltype(auto) operator()(T1 &&x,
                                                    T2 &&y) const noexcept {
    return std::views::iota(
        static_cast<std::common_type_t<T1, T2>>(std::forward<T1>(x)),
        static_cast<std::common_type_t<T1, T2>>(std::forward<T2>(y)));
  }
} make_iota_view{};

inline static constexpr struct {
  template <typename T>
  [[nodiscard]] constexpr decltype(auto) operator()(T &&x) const noexcept {
    if constexpr (std::is_convertible_v<T, std::string>)
      return std::ranges::fold_left(
          std::string{std::forward<T>(x)}, 0,
          [](auto &&x, auto &&y) -> u32 { return x * 37 + y; });
    else
      return std::forward<T>(x);
  }
} switch__{};

///
/// Built-in Functions
///

///
/// Numeric Functions
///

inline static constexpr struct {
  template <typename T__>
    requires std::is_arithmetic_v<T__>
  __attribute__((pure)) inline constexpr decltype(auto)
  operator()(T__ &&x__) const noexcept {
    return std::forward<decltype(x__)>(x__);
  }

  __attribute__((pure)) inline constexpr decltype(auto)
  operator()(auto &&x__) const noexcept {
    return cast__.operator()<f64>(std::forward<decltype(x__)>(x__));
  }

} as_numeric_arg__{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__, auto &&y__) const noexcept {
    return std::atan2(as_numeric_arg__(std::forward<decltype(x__)>(x__)),
                      as_numeric_arg__(std::forward<decltype(y__)>(y__)));
  }
} atan2_{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__) const noexcept {
    return std::cos(as_numeric_arg__(std::forward<decltype(x__)>(x__)));
  }
} cos_{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__) const noexcept {
    return std::exp(as_numeric_arg__(std::forward<decltype(x__)>(x__)));
  }
} exp_{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__) const noexcept {
    return cast__.operator()<i32>(std::forward<decltype(x__)>(x__));
  }
} int_{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__) const noexcept {
    return std::log(as_numeric_arg__(std::forward<decltype(x__)>(x__)));
  }
} log_{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__, auto &&y__) const noexcept {
    return std::pow(as_numeric_arg__(std::forward<decltype(x__)>(x__)),
                    as_numeric_arg__(std::forward<decltype(y__)>(y__)));
  }
} pow_{};

inline static struct {
  std::mt19937_64 gen__{};

  __attribute__((pure)) inline auto operator()() noexcept {
    return std::uniform_real_distribution<>{0, 1}(gen__);
  }
} rand_{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__) const noexcept {
    return std::sin(as_numeric_arg__(std::forward<decltype(x__)>(x__)));
  }
} sin_{};

inline static constexpr struct {
  __attribute__((pure)) inline constexpr auto
  operator()(auto &&x__) const noexcept {
    return std::sqrt(as_numeric_arg__(std::forward<decltype(x__)>(x__)));
  }
} sqrt_{};

inline static constexpr struct {
  inline void operator()() const noexcept {
    rand_.gen__.seed(std::random_device{}());
  }
} srand_{};

///
/// String Functions
///

inline static constexpr struct {
  constexpr void operator()(auto &&regexp__,
                            auto &&replacement__) const noexcept {
    std::string s__{};

    std::regex_replace(std::back_inserter(s__), std::cbegin(fields__.front()),
                       std::cend(fields__.front()), regexp__, replacement__);
    const auto n__{std::size(s__)};
    char *ptr__{(char *)malloc(sizeof(char) * n__)};
    std::ranges::move(s__, ptr__);

    if (!read_line__(ptr__, ptr__ + n__)) {
      fprintf(stderr, "error in gsub\n");
      exit(EXIT_FAILURE);
    };

    // Bit of a hack. I need the call to read_line__ to update the fields and
    // NF, and since I don't want to add extra overloads/new template
    // parameters, I'll keep the default where it also updates NR, and then
    // manually move it back since it didn't actually read a new record.
    --NR_;
  }

  constexpr void operator()(auto &&regexp__, auto &&replacement__,
                            auto &&target__) const noexcept {
    target__.clear();
    std::regex_replace(std::back_inserter(target__),
                       std::cbegin(fields__.front()),
                       std::cend(fields__.front()), regexp__, replacement__);
  }
} gsub_{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((pure)) inline constexpr std::string::size_type
  operator()(auto &&in__, auto &&find__) const noexcept
    requires(std::is_same_v<std::remove_cvref_t<decltype(in__)>, string> ||
             std::is_same_v<std::remove_cvref_t<decltype(in__)>,
                            std::string_view>) &&
            (std::is_same_v<std::remove_cvref_t<decltype(find__)>, string> ||
             std::is_same_v<std::remove_cvref_t<decltype(find__)>,
                            std::string_view>)
  {
    return std::ranges::search(in__, find__) - std::cbegin(in__);
  }

  [[nodiscard]] __attribute__((pure)) inline constexpr std::string::size_type
  operator()(auto &&in__, auto &&find__) const noexcept
    requires(std::is_same_v<std::remove_cvref_t<decltype(in__)>, string> ||
             std::is_same_v<std::remove_cvref_t<decltype(in__)>,
                            std::string_view>) &&
            std::is_same_v<std::remove_cvref_t<decltype(find__)>, char>
  {
    return std::ranges::find(in__, find__) - std::cbegin(in__);
  }
} index_{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((pure)) inline constexpr auto
  operator()(auto &&c__) const noexcept {
    return std::size(c__);
  }
} length_{};

inline static constexpr struct {
  [[nodiscard]] inline constexpr auto
  operator()(auto &&string__, auto &&regexp__) const noexcept {
    std::sregex_iterator first__{std::cbegin(string__), std::cend(string__),
                                 regexp__};
    if (first__ == std::sregex_iterator{})
      return RSTART_ = RLENGTH_ = -1;

    RSTART_ = first__->position();
    RLENGTH_ = std::size(first__->str());

    return RSTART_;
  }

  /*
    Not yet implemented.

    [[nodiscard]] inline constexpr auto
    operator()(auto &&string__, auto &&regexp__, auto &&array__) const noexcept;
  */
} match_{};

inline static constexpr struct {
  constexpr void operator()(auto &&string__, auto &&array__,
                            auto &&fieldsep__) const noexcept
    requires std::__is_same_uncvref<decltype(string__), string>::value
             && std::__is_same_uncvref<decltype(array__), slice<string>>::value
             && std::__is_same_uncvref<decltype(fieldsep__), char>::value
  {
    auto first__{std::cbegin(string__)}, next__{std::cbegin(string__)};
    for (; first__ != std::cend(string__);) {
      first__ =
          std::find_if(first__, std::cend(string__),
                       [fieldsep__](auto &&x__) constexpr noexcept -> bool {
                         return x__ != fieldsep__;
                       });

      if (first__ == std::cend(string__)) [[unlikely]]
        break;

      next__ = std::find(first__, std::cend(string__), fieldsep__);
      array__.emplace_back(first__, next__);
      first__ = next__;
    }
  }

  constexpr void operator()(auto &&string__, auto &&array__) const noexcept
    requires std::__is_same_uncvref<decltype(string__), string>::value
             && std::__is_same_uncvref<decltype(array__), slice<string>>::value
  {
    auto first__{std::cbegin(string__)}, next__{std::cbegin(string__)};
    for (; first__ != std::cend(string__);) {
      first__ = std::find_if(
          first__, std::cend(string__),
          [fieldsep__ = FS_](auto &&x__) constexpr noexcept -> bool {
            return x__ != fieldsep__;
          });

      if (first__ == std::cend(string__)) [[unlikely]]
        break;

      next__ = std::find(first__, std::cend(string__), FS_);
      array__.emplace_back(first__, next__);
      first__ = next__;
    }
  }
} split_{};

inline static constexpr struct {
  template <size_t Size__ = 256>
  __attribute__((pure)) string
  operator()(auto &&format__, auto &&...expressions__) const noexcept {
    std::string s__(Size__, ' ');
    s__.resize(std::snprintf(
        s__.data(), Size__, std::forward<decltype(format__)>(format__),
        std::forward<decltype(expressions__)>(expressions__)...));
    return s__;
  }
} sprintf_{};

///
/// CAWK-specific functions
///

inline static constexpr struct {
  inline constexpr void operator()(auto &&r__) const noexcept {
    std::ranges::sort(std::forward<decltype(r__)>(r__));
  }

  inline constexpr void operator()(auto &&r__, auto &&f__) const noexcept {
    std::ranges::sort(std::forward<decltype(r__)>(r__),
                      std::forward<decltype(f__)>(f__));
  }

  inline constexpr void operator()(auto &&r__, auto &&first__,
                                   auto &&last__) const noexcept {
    std::sort(std::begin(std::forward<decltype(r__)>(r__)) +
                  std::forward<decltype(first__)>(first__),
              std::begin(std::forward<decltype(r__)>(r__)) +
                  std::forward<decltype(last__)>(last__));
  }

  inline constexpr void operator()(auto &&r__, auto &&first__, auto &&last__,
                                   auto &&f__) const noexcept {
    std::sort(std::begin(std::forward<decltype(r__)>(r__)) +
                  std::forward<decltype(first__)>(first__),
              std::begin(std::forward<decltype(r__)>(r__)) +
                  std::forward<decltype(last__)>(last__),
              std::forward<decltype(f__)>(f__));
  }
} sort__{};

inline static constexpr struct {
  inline constexpr void operator()(auto &&x__, auto &&y__) const noexcept {
    std::swap(std::forward<decltype(x__)>(x__),
              std::forward<decltype(y__)>(y__));
  }
} swap__{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((__const__)) inline auto
  operator()(auto &&x__) const noexcept {
    return std::hash<std::decay_t<decltype(x__)>>{}(
        std::forward<std::decay_t<decltype(x__)>>(x__));
  }
} hash__{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((const)) inline constexpr auto
  operator()(auto &&r__) const noexcept {
    auto x__{std::forward<decltype(r__)>(r__)};
    std::sort(std::begin(x__), std::end(x__));
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
  [[nodiscard]] __attribute__((pure)) inline auto operator()() const noexcept {
    return std::chrono::high_resolution_clock::now();
  }
} time_point__{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((const)) inline auto
  operator()(auto &&x__, auto &&y__) const noexcept {
    return std::chrono::duration_cast<std::chrono::milliseconds>(y__ - x__)
        .count();
  }
} time_ms__{};

inline static constexpr struct {
  [[nodiscard]] __attribute__((const)) inline auto
  operator()(auto &&x__, auto &&y__) const noexcept {
    return std::chrono::duration_cast<std::chrono::microseconds>(y__ - x__)
        .count();
  }
} time_us__{};

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

inline static constexpr struct {
  inline auto operator()(auto &&...args__) const noexcept {
    return printf(std::forward<decltype(args__)>(args__)...);
  }
} printf_{};

} // namespace cawk

int main(int argc, char **argv) {
  cawk::parse_cmd_line__(argc, argv);
  if (cawk::positional__.front() != -1) {
    cawk::open__(argv[cawk::positional__.front()]);
    cawk::run__();
    cawk::close__();
  } else {
    cawk::run_begin__();
  }
}