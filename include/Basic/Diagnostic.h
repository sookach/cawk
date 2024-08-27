#pragma once

#include "Basic/SourceLocation.h"
#include "Exec/IO.h"

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
  using DiagVector = std::vector<std::pair<SourceRange, std::string>>;
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
  void addDiagnostic(DiagVector &Diagnostics, SourceRange SrcRange,
                     diag::DiagnosticKind Kind, T &&...Args) {
    Diagnostics.emplace_back(SrcRange,
                             formatDiagnostic(Kind, std::forward<T>(Args)...));
  }

public:
  template <typename... T>
  void addError(SourceRange SrcRange, diag::DiagnosticKind Kind, T &&...Args) {
    addDiagnostic(ErrorDiagnostics, SrcRange, Kind, std::forward<T>(Args)...);
  }

  template <typename... T>
  void addWarning(SourceRange SrcRange, diag::DiagnosticKind Kind,
                  T &&...Args) {
    addDiagnostic(WarningDiagnostics, SrcRange, Kind, std::forward<T>(Args)...);
  }

  void clearErrors() { ErrorDiagnostics.clear(); }

  void clearWarnings() { WarningDiagnostics.clear(); }

  void printErrors(std::string_view Source) {
    for (const auto &[SrcRange, Error] : ErrorDiagnostics) {
      errs().printf("error: %s\n", Error.c_str());
      auto LineNumber =
          std::count(std::cbegin(Source), SrcRange.getBegin(), '\n') + 1;
      std::string SourceLines("   " + std::to_string(LineNumber) + " | ");
      for (auto It = SrcRange.getBegin(); It != SrcRange.getEnd(); ++It) {
        SourceLines.push_back(*It);
        if (*It == '\n' && It != SrcRange.getEnd() - 1)
          SourceLines += "   " + std::to_string(++LineNumber) + " | ";
      }
      errs().printf("%s\n", SourceLines.c_str());
    }
  }
};
} // namespace cawk