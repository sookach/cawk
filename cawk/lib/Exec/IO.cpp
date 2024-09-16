#include "Exec/IO.h"

#include <algorithm>
#include <array>
#include <cassert>
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

std::string InputFile::toString() {
  return std::string(std::cbegin(Buffer), End);
}

OutputFile::OutputFile(std::string Pathname) {
  FilePtr = std::fopen(Pathname.c_str(), "w");
}

OutputFile::OutputFile(FILE *FilePtr) : FilePtr(FilePtr) {}

void OutputFile::put(char C) {
  if (Curr == End)
    flush();
  *Curr++ = C;
}

void OutputFile::put(std::string S) {
  for (char C : S)
    put(C);
}

void OutputFile::flush() {
  std::fwrite(std::data(Buffer), 1, Curr - std::begin(Buffer), FilePtr);
  Curr = std::begin(Buffer);
}

void OutputFile::close() {
  flush();
  std::fclose(FilePtr);
}