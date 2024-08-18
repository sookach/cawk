#pragma once

namespace type {
enum TypeKind { null, primitive, array, any };

inline const char *toString(TypeKind T) {
  switch (T) {
  case null:
    return "null";
  case primitive:
    return "primitive";
  case array:
    return "array";
  case any:
    return "any";
  }
  return "unknown";
}
} // namespace type