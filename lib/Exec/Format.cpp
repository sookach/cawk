#include "Exec/Format.h"
#include "Support/Support.h"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdio>

using namespace cawk;

std::string cawk::format(std::string FormatString, std::vector<Value> Args) {
  static constexpr std::string::size_type MaxSize = 10;
  std::array<char, MaxSize> Buffer;

  auto It = std::begin(FormatString);
  auto End = std::end(FormatString);
  auto BufferIt = std::begin(Buffer);

  for (Value &A : Args) {
    auto Next = std::find(It, End, '%');
    if (Next == End) {
      cawk_unreachable("Invalid format string.");
      exit(EXIT_FAILURE);
    }

    std::copy(It, Next, BufferIt);
    BufferIt += Next - It;
    It = Next;

    if (*(It + 1) == '%') {
      *(BufferIt++) = '%';
      It += 2;
      continue;
    }

    auto WriteFormat = [&](const char *Fmt, auto Arg) {
      auto N = std::snprintf(
          BufferIt, MaxSize - (BufferIt - std::begin(Buffer)), Fmt, Arg);
      if (N < 0) {
        perror("");
        exit(errno);
      }
      BufferIt += N;
    };

    switch (++It; *(It++)) {
    default:
      cawk_unreachable("Invalid format string.");
      break;
    case '%':
      *(BufferIt++) = '%';
      break;
    case 'c':
      WriteFormat("%c", A.getType() == NumberTy
                            ? A.getAs<NumberTy>()
                            : static_cast<int>(A.getAs<StringTy>().front()));
      break;
    case 's': {
      auto S = A.getAs<StringTy>();
      WriteFormat("%s", S.c_str());
      break;
    }
    case 'd':
    case 'i':
      WriteFormat("%d", static_cast<int>(A.getAs<NumberTy>()));
      break;
    case 'o':
      WriteFormat("%o", static_cast<unsigned int>(A.getAs<NumberTy>()));
      break;
    case 'x':
    case 'X':
      WriteFormat("%x", static_cast<unsigned int>(A.getAs<NumberTy>()));
      break;
    case 'u':
      WriteFormat("%u", static_cast<unsigned int>(A.getAs<NumberTy>()));
      break;
    case 'f':
    case 'F':
      WriteFormat("%f", A.getAs<NumberTy>());
      break;
    case 'e':
    case 'E':
      WriteFormat("%e", A.getAs<NumberTy>());
      break;
    case 'a':
    case 'A':
      WriteFormat("%a", A.getAs<NumberTy>());
      break;
    case 'g':
    case 'G':
      WriteFormat("%g", A.getAs<NumberTy>());
      break;
    case 'h':
      switch (*It++) {
      default:
        cawk_unreachable("Invalid format string.");
      case 'h':
        switch (*It++) {
        default:
          cawk_unreachable("Invalid format string.");
        case 'd':
        case 'i':
          WriteFormat("%hhd", static_cast<signed char>(A.getAs<NumberTy>()));
          break;
        case 'o':
          WriteFormat("%hho", static_cast<unsigned char>(A.getAs<NumberTy>()));
          break;
        case 'x':
        case 'X':
          WriteFormat("%hhx", static_cast<unsigned char>(A.getAs<NumberTy>()));
          break;
        case 'u':
          WriteFormat("%hhu", static_cast<unsigned char>(A.getAs<NumberTy>()));
        }
        break;
      case 'd':
      case 'i':
        WriteFormat("%hd", static_cast<short>(A.getAs<NumberTy>()));
        break;
      case 'o':
        WriteFormat("%ho", static_cast<unsigned short>(A.getAs<NumberTy>()));
        break;
      case 'x':
      case 'X':
        WriteFormat("%hx", static_cast<unsigned short>(A.getAs<NumberTy>()));
        break;
      case 'u':
        WriteFormat("%hu", static_cast<unsigned short>(A.getAs<NumberTy>()));
      }
      break;
    case 'l':
      switch (*It++) {
      default:
        cawk_unreachable("Invalid format string.");
      case 'c':
        WriteFormat(
            "%lc", A.getType() == NumberTy
                       ? static_cast<std::wint_t>(A.getAs<NumberTy>())
                       : static_cast<std::wint_t>(A.getAs<StringTy>().front()));
        break;
      case 's': {
        std::wstring S;
        std::ranges::transform(A.getAs<StringTy>(), std::back_inserter(S),
                               [](char C) { return static_cast<wchar_t>(C); });
        WriteFormat("%ls", S.c_str());
        break;
      }
      case 'd':
      case 'i':
        WriteFormat("%ld", static_cast<long>(A.getAs<NumberTy>()));
        break;
      case 'o':
        WriteFormat("%lo", static_cast<unsigned long>(A.getAs<NumberTy>()));
        break;
      case 'x':
      case 'X':
        WriteFormat("%lx", static_cast<unsigned long>(A.getAs<NumberTy>()));
        break;
      case 'u':
        WriteFormat("%lu", static_cast<unsigned long>(A.getAs<NumberTy>()));
        break;
      case 'f':
      case 'F':
        WriteFormat("%lf", A.getAs<NumberTy>());
        break;
      case 'e':
      case 'E':
        WriteFormat("%le", A.getAs<NumberTy>());
        break;
      case 'a':
      case 'A':
        WriteFormat("%la", A.getAs<NumberTy>());
        break;
      case 'g':
      case 'G':
        WriteFormat("%lg", A.getAs<NumberTy>());
        break;
      case 'l':
        switch (*It++) {
        default:
          cawk_unreachable("Invalid format string.");
        case 'd':
        case 'i':
          WriteFormat("%lld", static_cast<long long>(A.getAs<NumberTy>()));
          break;
        case 'o':
          WriteFormat("%llo",
                      static_cast<unsigned long long>(A.getAs<NumberTy>()));
          break;
        case 'x':
        case 'X':
          WriteFormat("%llx",
                      static_cast<unsigned long long>(A.getAs<NumberTy>()));
          break;
        case 'u':
          WriteFormat("%llu",
                      static_cast<unsigned long long>(A.getAs<NumberTy>()));
        }
        break;
      }
      break;
    case 'j':
      switch (*It++) {
      default:
        cawk_unreachable("Invalid format string.");
      case 'd':
      case 'i':
        WriteFormat("%jd", static_cast<std::intmax_t>(A.getAs<NumberTy>()));
        break;
      case 'o':
        WriteFormat("%jo", static_cast<std::uintmax_t>(A.getAs<NumberTy>()));
        break;
      case 'x':
      case 'X':
        WriteFormat("%jx", static_cast<std::uintmax_t>(A.getAs<NumberTy>()));
        break;
      case 'u':
        WriteFormat("%ju", static_cast<std::uintmax_t>(A.getAs<NumberTy>()));
      }
      break;
    case 'z':
      switch (*It++) {
      default:
        cawk_unreachable("Invalid format string.");
      case 'd':
      case 'i':
        WriteFormat(
            "%zd",
            static_cast<ssize_t>(A.getAs<NumberTy>())); // signed std::size_t
        break;
      case 'o':
        WriteFormat("%zo", static_cast<std::size_t>(A.getAs<NumberTy>()));
        break;
      case 'x':
      case 'X':
        WriteFormat("%zx", static_cast<std::size_t>(A.getAs<NumberTy>()));
        break;
      case 'u':
        WriteFormat("%zu", static_cast<std::size_t>(A.getAs<NumberTy>()));
      }
      break;
    case 't':
      switch (*It++) {
      default:
        cawk_unreachable("Invalid format string.");
      case 'd':
      case 'i':
        WriteFormat("%td", static_cast<std::ptrdiff_t>(A.getAs<NumberTy>()));
        break;
      case 'o':
        WriteFormat("%zo", static_cast<std::size_t>(
                               A.getAs<NumberTy>())); // unsigned std::ptrdiff_t
        break;
      case 'x':
      case 'X':
        WriteFormat("%zx", static_cast<std::size_t>(
                               A.getAs<NumberTy>())); // unsigned std::ptrdiff_t
        break;
      case 'u':
        WriteFormat("%zu", static_cast<std::size_t>(
                               A.getAs<NumberTy>())); // unsigned std::ptrdiff_t
      }
      break;
    case 'L':
      switch (*It++) {
      default:
        cawk_unreachable("Invalid format string.");
      case 'f':
      case 'F':
        WriteFormat("%Lf", static_cast<long double>(A.getAs<NumberTy>()));
        break;
      case 'e':
      case 'E':
        WriteFormat("%Le", static_cast<long double>(A.getAs<NumberTy>()));
        break;
      case 'a':
      case 'A':
        WriteFormat("%La", static_cast<long double>(A.getAs<NumberTy>()));
        break;
      case 'g':
      case 'G':
        WriteFormat("%Lg", static_cast<long double>(A.getAs<NumberTy>()));
      }
    }
  }

  auto Next = std::find(It, End, '%');
  std::move(It, Next, BufferIt);
  BufferIt += Next - It;

  // if (Next != End) {
  //   cawk_unreachable("Invalid format string.");
  //   exit(EXIT_FAILURE);
  // }

  return {std::begin(Buffer), BufferIt};
}

template <typename... T>
std::string cawk::format(std::string FormatString, T... Args) {
  return format(FormatString, std::vector(Args...));
}