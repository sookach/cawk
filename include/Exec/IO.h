#pragma once

#include <array>
#include <cstdio>
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
  void put(char C);
  void put(std::string S);
  void flush();
  void close();
  template <typename... Args> void printf(const char *Format, Args &&...A) {
    std::fprintf(FilePtr, Format, std::forward<Args>(A)...);
  }
  template <typename T, typename... Args>
  void print(T &&First, Args &&...Rest) {
    put(std::forward<T>(First));
    (put(std::forward<Args>(Rest)), ...);
  }
};

inline OutputFile &outs() {
  static OutputFile Out(stdout);
  return Out;
}

} // namespace cawk