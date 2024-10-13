//===- Object.h - The object interface ------------------------------------===//
//
// This file defines the Object class.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "cawk/Support/Support.h"
#include <string>

namespace cawk {

enum ObjectTy {
  StringObj,
};

struct Object {
  ObjectTy Type;
  bool is(ObjectTy Ty) const { return Type == Ty; }
};

struct StringObject {
  Object Obj = Object(StringObj);
  std::string String;

  StringObject(std::string String) : String(String) {}

  double strtod() const { return std::strtod(String.c_str(), nullptr); }

  static Object *Create(std::string String) {
    return reinterpret_cast<Object *>(new StringObject(String));
  }
};

namespace obj {
template <ObjectTy T> auto *cast(Object *O) {
  if constexpr (T == StringObj)
    return reinterpret_cast<StringObject *>(O);
}
} // namespace obj

} // namespace cawk