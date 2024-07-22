#include "Exec/IO.h"

#include <cerrno>

using namespace cawk;

cawk::IO::IO(std::string Filepath, IO::ModeKind Mode) {
  auto ModeString = toString(Mode);
  File = std::fopen(Filepath.c_str(), ModeString.c_str());
  Error = errno;
}

void cawk::IO::write(std::string S) {
  std::fprintf(File, "%s", S.c_str());
  Error = errno;
}

std::string cawk::IO::getLine(char Delim) {
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