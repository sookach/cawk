#pragma once

#include "Support/Support.h"

#include <cassert>
#include <string>
#include <unordered_map>

namespace cawk {

class Value {
  enum ValueKind { VK_Null, VK_Number, VK_String, VK_Array };
  ValueKind Kind = VK_Null;
  double NumberValue;
  std::string StringValue;
  struct Hash {
    std::size_t operator()(const Value *) const;
  };
  std::unordered_map<Value *, Value *, Hash> ArrayValue;

public:
  Value(double Value) : Kind(VK_Number), NumberValue(Value) {}

  Value(std::string_view Value) : Kind(VK_String), StringValue(Value) {}

  Value(Value *Key, Value *Val) : Kind(VK_Array) { ArrayValue[Key] = Val; }

  ValueKind getKind() const { return Kind; }

  double getNumber() const { return NumberValue; }

  std::string getString() const { return StringValue; }

  std::unordered_map<Value *, Value *> getArray() const { return ArrayValue; }

  double toNumber() {
    switch (Kind) {
    case VK_Number:
      return NumberValue;
    case VK_String:
      try {
        return std::stod(StringValue);
      } catch (...) {
        return 0;
      }
      return NumberValue;
    case VK_Array:
      assert("Cannot convert non-scalar to scalar value.");
      exit(EXIT_FAILURE);
    }
  }

  std::string toString() {
    switch (Kind) {
    case VK_Number:
      return std::to_string(NumberValue);
    case VK_String:
      return StringValue;
    case VK_Array:
      assert("Cannot convert non-scalar to scalar value.");
      exit(EXIT_FAILURE);
    }
  }

  std::unordered_map<Value *, Value *, Hash> toArray() {
    if (Kind != VK_Array) {
      assert("Cannot convert scalr to non-scalar value.");
      exit(EXIT_FAILURE);
    }
    return ArrayValue;
  }

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

  Value &operator[](const Value *V) { return *getArray()[V]; }

  Value &operator[](const Value &V) { return operator[](&V); }

  Value &operator++() {
    makeNumber();
    ++NumberValue;
    return *this;
  }

  Value operator++(int) {
    makeNumber();
    auto Temp = *this;
    ++NumberValue;
    return Temp;
  }

  friend Value operator+(const Value &, const Value &);
  friend Value operator-(const Value &, const Value &);
  friend Value operator*(const Value &, const Value &);
  friend Value operator/(const Value &, const Value &);

  operator bool() const {
    switch (Kind) {
    case VK_Number:
      return NumberValue != 0;
    case VK_String:
      return !std::empty(StringValue);
    case VK_Array:
      return true;
    }
  }

  void makeNumber() {
    NumberValue = toNumber();
    Kind = VK_Number;
  }

  void makeString() {
    StringValue = toString();
    Kind = VK_String;
  }

  void makeArray() {
    ArrayValue = toArray();
    Kind = VK_Array;
  }

  void erase(Value &V) {
    assert(ValueKind == VK_Array && "Cannot erase from non-array.");
    ArrayValue.erase(&V);
  }

  void clear() {
    assert(ValueKind == VK_Array && "Cannot clear a non-array.");
    ArrayValue.clear();
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

std::size_t Value::Hash::operator()(const Value *V) const {
  switch (V->getKind()) {
  case VK_Number:
    return std::hash<double>()(V->getNumber());
  case VK_String:
    return std::hash<std::string>()(V->getString());
  case VK_Array:
    assert("awk: can't read value of a; it's an array name.");
    exit(EXIT_FAILURE);
  }
}

} // namespace cawk
