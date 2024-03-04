#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <execution>
#include <fcntl.h>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <ranges>
#include <regex>
#include <string>
#include <string_view>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <vector>

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

template <typename T__>
concept regex__ =
    requires(T__ t__) { requires std::is_convertible_v<T__, string>; };

inline static constexpr struct {
  [[nodiscard]] __attribute__((pure)) inline bool
  operator()(regex__ auto &&x__, regex__ auto &&y__) const noexcept {
    return std::regex_search(x__, std::regex{y__});
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

std::vector<std::span<char>> fields__{};
std::string record__{};

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

uint64_t NR{}, NF{};
bool BEGIN{true}, END{}, mid__{false};
char *bytes__{}, *prev__{}, *next__{};
off_t size__{};

inline static struct {
  void operator()(std::string_view filename__) const noexcept {
    const int fd__{open(filename__.data(), O_RDONLY)};
    if (fd__ == -1) [[unlikely]] {
      perror("");
      exit(errno);
    }

    struct stat fileinfo__ {};
    if (fstat(fd__, &fileinfo__) == -1) [[unlikely]] {
      perror("");
      exit(errno);
    }

    size__ = fileinfo__.st_size;
    if ((next__ = bytes__ = (char *)mmap(NULL, size__, PROT_READ, MAP_SHARED,
                                         fd__, 0)) == MAP_FAILED) [[unlikely]] {
      perror("");
      exit(errno);
    }

    close(fd__);
  }
} mmap__{};

inline struct {
  [[nodiscard]] bool operator()(std::istream &is__) const noexcept {
    fields__.clear();
    NF = 0;
    fields__.emplace_back();

    fields__.clear();

    if (!std::getline(is__, record__))
      return false;

    fields__.emplace_back(std::begin(record__), std::end(record__));

    string::size_type first__{}, last__{};
    bool curr__{};

    for (string::size_type i__{}; auto &&x__ : record__) {
      if (!std::isspace(x__) && !curr__) {
        first__ = i__;
        curr__ = true;
      } else if (std::isspace(x__) && curr__) {
        last__ = i__;
        fields__.emplace_back(record__.data() + first__, last__ - first__);
      }
      ++i__;
    }

    if (curr__)
      fields__.emplace_back(record__.data() + first__,
                            std::size(record__) - first__);

    ++NR;
    return true;
  }
} read_line__{};

inline static constexpr struct {
  [[nodiscard]] bool operator()() const noexcept {
    NF = 0;
    fields__.clear();

    if (next__ >= bytes__ + size__) [[unlikely]]
      return false;

    ++NR;

    char *const start__ = next__;

    for (fields__.emplace_back();
         next__ != bytes__ + size__ && *next__ != '\n';) {
      prev__ = std::find_if_not(next__, bytes__ + size__, ::isspace);
      next__ = std::find_if(prev__, bytes__ + size__, ::isspace);
      fields__.emplace_back(prev__, next__);
    }

    fields__.front() = {start__, next__};

    ++next__;
    return true;
  }
} mmap_read_line__{};

inline std::function<void(void)> run_begin__{};
inline std::function<void(void)> run_mid__{};
inline std::function<void(void)> run_end__{};

inline struct {
  void operator()(std::istream &is__) const noexcept {
    std::invoke(run_begin__);
    for (; read_line__(is__);)
      run_mid__();
    run_end__();
  }
} run__{};

inline static constexpr struct {
  void operator()() const noexcept {
    std::invoke(run_begin__);
    for (; mmap_read_line__();)
      run_mid__();
    run_end__();
  }
} mmap_run__{};

inline void init__() noexcept;

} // namespace cawk

int main(int argc, char **argv) {
  cawk::init__();
  if (argc == 2) {
    cawk::mmap__(argv[1]);
    cawk::mmap_run__();
  } else {
    cawk::run_end__();
    cawk::run_begin__();
  }
}