#pragma once

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdio>
#include <string>
#include <vector>

namespace cawk {

namespace diag {
enum DiagnosticKind {
#define DIAG(LEVEL, ID, TEXT) ID,
#include "Basic/Diagnostic.def"
};
std::string_view getDiagnosticText(diag::DiagnosticKind Kind);
} // namespace diag

class Diagnostic {
  using DiagVector = std::vector<std::pair<std::size_t, std::string>>;
  DiagVector ErrorDiagnostics;
  DiagVector WarningDiagnostics;

  std::string formatDiagnostic(std::string_view Text) {
    assert(!std::ranges::contains(Text, '%') && "missing format argument");
    return std::string(Text);
  }

  template <typename T, typename... ArgsTy>
  std::string formatDiagnostic(std::string_view Text, T &&Arg,
                               ArgsTy &&...Args) {
    auto I = std::ranges::find(Text, '%');
    assert(I != std::cend(Text) && "missing format specifier");
    std::string FormattedText(std::cbegin(Text), I);
    if constexpr (std::is_same_v<std::decay_t<T>, std::string>)
      FormattedText += Arg;
    else if constexpr (std::is_same_v<std::decay_t<T>, const char *>)
      FormattedText += std::string(Arg);
    else if constexpr (std::is_same_v<std::decay_t<T>, std::string_view>)
      FormattedText += std::string(Arg);
    else
      FormattedText += std::to_string(Arg);
    return FormattedText +
           formatDiagnostic(std::string_view(I + 1, std::cend(Text)),
                            std::forward<ArgsTy>(Args)...);
  }

  template <typename... T>
  std::string formatDiagnostic(diag::DiagnosticKind Kind, T &&...Args) {
    return formatDiagnostic(diag::getDiagnosticText(Kind),
                            std::forward<T>(Args)...);
  }

  template <typename... T>
  void addDiagnostic(DiagVector &Diagnostics, std::size_t Line,
                     diag::DiagnosticKind Kind, T &&...Args) {
    Diagnostics.emplace_back(Line,
                             formatDiagnostic(Kind, std::forward<T>(Args)...));
  }

public:
  template <typename... T>
  void addError(std::size_t Line, diag::DiagnosticKind Kind, T &&...Args) {
    addDiagnostic(ErrorDiagnostics, Line, Kind, std::forward<T>(Args)...);
  }

  template <typename... T>
  void addWarning(std::size_t Line, diag::DiagnosticKind Kind, T &&...Args) {
    addDiagnostic(WarningDiagnostics, Line, Kind, std::forward<T>(Args)...);
  }

  void clearErrors() { ErrorDiagnostics.clear(); }

  void clearWarnings() { WarningDiagnostics.clear(); }

  void printErrors(std::string_view Source) {
    for (const auto &[Line, Error] : ErrorDiagnostics) {
      std::fprintf(stderr, "error: %s\n", Error.c_str());
      std::fprintf(stderr, "  %lu | %s\n", Line,
                   getSourceLine(Source, Line).c_str());
    }
  }

  std::string getSourceLine(std::string_view Source, std::size_t Line) {
    auto Begin = std::cbegin(Source);
    for (std::size_t I = 1; I != Line; ++I)
      Begin = std::find(Begin, std::cend(Source), '\n') + 1;
    auto End = std::find(Begin, std::cend(Source), '\n');
    return std::string(Begin, End == std::cend(Source) ? End - 1 : End);
  }
};
} // namespace cawk