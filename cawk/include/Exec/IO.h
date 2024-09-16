#pragma once

#include <array>
#include <cstdio>
#include <cstdlib>
#include <string>

namespace cawk {

class InputFile {
  FILE *FilePtr;
  std::array<char, 4096> Buffer;
  decltype(Buffer)::const_iterator Curr = std::begin(Buffer);
  decltype(Buffer)::const_iterator End;

public:
  InputFile(std::string Pathname);
  std::string getLine(char Delim = '\n');
  bool isEOF();
  std::string toString();
};

class OutputFile {
  FILE *FilePtr;
  std::array<char, 4096> Buffer;
  decltype(Buffer)::iterator Curr = std::begin(Buffer);
  decltype(Buffer)::iterator End = std::end(Buffer);

public:
  OutputFile(std::string Pathname);
  OutputFile(FILE *FilePtr);
  OutputFile &put(char C);
  OutputFile &put(std::string S);
  OutputFile &flush();
  void close();
  template <typename... Args>
  OutputFile &printf(const char *Format, Args &&...A) {
    if constexpr (sizeof...(A) == 0)
      std::fprintf(FilePtr, "%s", Format);
    else
      std::fprintf(FilePtr, Format, std::forward<Args>(A)...);
    return *this;
  }
  template <typename T, typename... Args>
  OutputFile &print(T &&First, Args &&...Rest) {
    put(std::forward<T>(First));
    (put(std::forward<Args>(Rest)), ...);
  }
};

inline OutputFile &outs() {
  static OutputFile Out(stdout);
  return Out;
}

inline OutputFile &errs() {
  static OutputFile Err(stderr);
  return Err;
}

template <typename... T> [[noreturn]] inline void cawk_fatal(T &&...Args) {
  errs().print(std::forward<T>(Args)...);
  errs().flush();
  std::exit(1);
}

} // namespace cawk