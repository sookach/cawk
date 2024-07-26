#pragma once

#include <cstdio>
#include <string>
#include <array>

namespace cawk {

class InputFile {
  FILE *FilePtr;
  std::array<char, 4096> Buffer;
  decltype(Buffer)::const_iterator BufferIt = std::begin(Buffer);

public:
  InputFile(std::string Pathname);
  std::string getLine(char Delim = '\n');
  bool isEOF();
};

} // namespace cawk