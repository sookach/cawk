#include "Exec/IO.h"

#include <algorithm>
#include <array>
#include <cerrno>

using namespace cawk;

InputFile::InputFile(std::string Pathname) {
  FilePtr = std::fopen(Pathname.c_str(), "r");
  for (auto It = std::begin(Buffer); !std::feof(FilePtr); ++It)
    *It = std::getc(FilePtr);
  BufferIt = std::begin(Buffer);
}

std::string InputFile::getLine(char Delim) {
  BufferIt = std::find_if(BufferIt, std::cend(Buffer),
                          [Delim](char C) { return C != Delim; });
  auto Next = std::find(BufferIt, std::cend(Buffer), Delim);

  return std::string(std::exchange(BufferIt, Next), Next);
}

bool InputFile::isEOF() { return BufferIt == std::end(Buffer); }