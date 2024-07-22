#pragma once

#include <cstdio>
#include <string>

namespace cawk {
class IO {
public:
  enum ModeKind {
    Read,
    Write,
    ReadWrite,
    ReadWriteCreate,
  };

private:
  FILE *File;
  errno_t Error;

  static std::string toString(ModeKind Mode) {
    switch (Mode) {
    case Read:
      return "r";
    case Write:
      return "w";
    case ReadWrite:
      return "r+";
    case ReadWriteCreate:
      return "w+";
    }
  }

public:
  IO(std::string Pathname, ModeKind Mode);

  void write(std::string S);
  std::string getLine(char Delim = '\n');
  bool hasError();
  errno_t getError();
};
} // namespace cawk