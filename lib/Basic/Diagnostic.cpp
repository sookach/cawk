#include "Basic/Diagnostic.h"

using namespace cawk::diag;

std::string_view cawk::diag::getDiagnosticText(diag::DiagnosticKind Kind) {
  switch (Kind) {
#define DIAG(LEVEL, KIND, TEXT)                                                \
  case KIND:                                                                   \
    return #TEXT;
#include "Basic/Diagnostic.def"
  }
}