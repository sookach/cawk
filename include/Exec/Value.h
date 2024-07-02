#pragma once

#include "Support/Support.h"

#include <cassert>
#include <string>
#include <unordered_map>

namespace cawk {

class Value {
  enum ValueKind { VK_Number, VK_String, VK_Array };
  ValueKind Kind;
  double NumberValue;
  std::string StringValue;
  std::unordered_map<Value *, Value *> ArrayValue;

public:
  Value(double Value) : Kind(VK_Number), NumberValue(Value) {}

  Value(std::string_view Value) : Kind(VK_String), StringValue(Value) {}

  Value(Value *Key, Value *Val) : Kind(VK_Array) { ArrayValue[Key] = Val; }

  ValueKind getKind() const { return Kind; }

  double getNumber() const { return NumberValue; }

  std::string getString() const { return StringValue; }

  std::unordered_map<Value *, Value *> getArray() const { return ArrayValue; }

  Value &operator+=(const Value &V) {
    makeNumber();
    if (V.getKind() == VK_Number) {
      NumberValue += V.getNumber();
    } else {
      auto Temp = V;
      Temp.makeNumber();
      NumberValue += Temp.getNumber();
    }

    return *this;
  }

  Value &operator-=(const Value &V) {
    makeNumber();
    if (V.getKind() == VK_Number) {
      NumberValue -= V.getNumber();
    } else {
      auto Temp = V;
      Temp.makeNumber();
      NumberValue -= Temp.getNumber();
    }

    return *this;
  }

  Value &operator*=(const Value &V) {
    makeNumber();
    if (V.getKind() == VK_Number) {
      NumberValue *= V.getNumber();
    } else {
      auto Temp = V;
      Temp.makeNumber();
      NumberValue *= Temp.getNumber();
    }

    return *this;
  }

  Value &operator/=(const Value &V) {
    makeNumber();
    if (V.getKind() == VK_Number) {
      NumberValue /= V.getNumber();
    } else {
      auto Temp = V;
      Temp.makeNumber();
      NumberValue /= Temp.getNumber();
    }

    return *this;
  }

  friend Value operator+(const Value &, const Value &);
  friend Value operator-(const Value &, const Value &);
  friend Value operator*(const Value &, const Value &);
  friend Value operator/(const Value &, const Value &);

  void makeNumber() {
    switch (Kind) {
    case VK_Number:
      return;
    case VK_String:
      try {
        Kind = VK_Number;
        NumberValue = std::stod(StringValue);
      } catch (...) {
        NumberValue = 0;
      }
      return;
    case VK_Array:
      assert("Cannot convert non-scalar to scalar value");
      exit(EXIT_FAILURE);
    }
  }

  void makeString() {
    switch (Kind) {
    case VK_Number:
      Kind = VK_String;
      StringValue = std::to_string(NumberValue);
      return;
    case VK_String:
      return;
    case VK_Array:
      assert("Cannot convert non-scalar to scalar value");
      exit(EXIT_FAILURE);
    }
  }
};

inline Value operator+(const Value &V1, const Value &V2) {
  auto V3 = V1;
  return V3 += V2;
}

inline Value operator-(const Value &V1, const Value &V2) {
  auto V3 = V1;
  return V3 -= V2;
}

inline Value operator*(const Value &V1, const Value &V2) {
  auto V3 = V1;
  return V3 *= V2;
}

inline Value operator/(const Value &V1, const Value &V2) {
  auto V3 = V1;
  return V3 /= V2;
}

} // namespace cawk
