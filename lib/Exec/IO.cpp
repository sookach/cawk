#include "Exec/IO.h"

#include <cerrno>

using namespace cawk;

IO::IO(std::string Filepath, IO::ModeKind Mode) {
  auto ModeString = toString(Mode);
  File = std::fopen(Filepath.c_str(), ModeString.c_str());
  Error = errno;
}

void IO::write(std::string S) {
  std::fprintf(File, "%s", S.c_str());
  Error = errno;
}

std::string IO::getLine(char Delim) {
  std::string S;

  for (bool Break = false; !Break;) {
    switch (S.push_back(std::getc(File)); S.back()) {
    default:
      if (S.back() != Delim)
        break;
    case EOF:
      S.pop_back();
      Break = true;
    }
  }

  return S;
}

bool IO::hasError() { return getError() != 0; }

errno_t IO::getError() { return Error; }

bool IO::isEOF() { return std::feof(File) != 0; }