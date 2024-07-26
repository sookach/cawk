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

} // namespace cawk