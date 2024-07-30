#pragma once

#include <array>
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
  std::vector<std::string> ErrorDiagnostics;
  std::vector<std::string> WarningDiagnostics;

  template <typename... T>
  void addDiagnostic(std::vector<std::string> &Diagnostics,
                     diag::DiagnosticKind Kind, T &&...Args) {
    std::array<char, 1'024> Buffer;
    auto N = std::snprintf(std::begin(Buffer), std::size(Buffer),
                           diag::getDiagnosticText(Kind).data(),
                           std::forward<T>(Args)...);
    Diagnostics.emplace_back(std::cbegin(Buffer), std::cbegin(Buffer) + N);
  }

public:
  template <typename... T>
  void addError(diag::DiagnosticKind Kind, T &&...Args) {
    addDiagnostic(ErrorDiagnostics, Kind, std::forward<T>(Args)...);
  }

  template <typename... T>
  void addWarning(diag::DiagnosticKind Kind, T &&...Args) {
    addDiagnostic(WarningDiagnostics, Kind, std::forward<T>(Args)...);
  }
};
} // namespace cawk