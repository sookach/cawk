#include "Exec/IO.h"

#include <algorithm>
#include <array>
#include <cerrno>

using namespace cawk;

InputFile::InputFile(std::string Pathname) {
  FilePtr = std::fopen(Pathname.c_str(), "r");
  auto It = std::begin(Buffer);
  for (; !std::feof(FilePtr); ++It)
    *It = std::getc(FilePtr);
  End = It;
  Curr = std::begin(Buffer);
}

std::string InputFile::getLine(char Delim) {
  Curr = std::find_if(Curr, End, [Delim](char C) { return C != Delim; });
  auto Next = std::find(Curr, End, Delim);

  return std::string(std::exchange(Curr, Next), Next);
}

bool InputFile::isEOF() { return Curr == End; }