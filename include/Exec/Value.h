#pragma once

#include <cassert>
#include <cstdlib>
#include <string>
#include <unordered_map>

namespace cawk {

class Value {
public:
  enum ValueKind { VK_Null, VK_Number, VK_String, VK_Array };

private:
  ValueKind Kind = VK_Null;
  double NumberValue;
  std::string StringValue;
  std::unordered_map<std::size_t, Value *> ArrayValue;

  struct {
    std::size_t operator()(const Value *V) const {
      switch (V->getKind()) {
      case VK_Null:
        return std::hash<const Value *>()(V);
      case VK_Number:
        return std::hash<double>()(V->getNumber());
      case VK_String:
        return std::hash<std::string>()(V->getString());
      case VK_Array:
        assert("awk: can't read value of a; it's an array name.");
        exit(EXIT_FAILURE);
      }
    }

    std::size_t operator()(const Value &V) const {
      switch (V.getKind()) {
      case VK_Null:
        return std::hash<const Value *>()(&V);
      case VK_Number:
        return std::hash<double>()(V.getNumber());
      case VK_String:
        return std::hash<std::string>()(V.getString());
      case VK_Array:
        assert("awk: can't read value of a; it's an array name.");
        exit(EXIT_FAILURE);
      }
    }
  } Hash;

public:
  Value() = default;

  Value(double Value) : Kind(VK_Number), NumberValue(Value) {}

  Value(std::string_view Value) : Kind(VK_String), StringValue(Value) {}

  Value(Value *Key, Value *Val) : Kind(VK_Array) {
    ArrayValue[Hash(Key)] = Val;
  }

  ValueKind getKind() const { return Kind; }

  double getNumber() const { return NumberValue; }

  std::string getString() const { return StringValue; }

  std::unordered_map<std::size_t, Value *> getArray() const {
    return ArrayValue;
  }

  double toNumber() const {
    switch (Kind) {
    case VK_Null:
      return 0;
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

  std::string toString() const {
    switch (Kind) {
    case VK_Null:
      return "";
    case VK_Number: {
      auto S = std::to_string(NumberValue);
      for (; S.back() == '0'; S.pop_back())
        ;

      if (S.back() == '.')
        S.pop_back();

      return S;
    }
    case VK_String:
      return StringValue;
    case VK_Array:
      assert("Cannot convert non-scalar to scalar value.");
      exit(EXIT_FAILURE);
    }
  }

  std::unordered_map<std::size_t, Value *> toArray() const {
    if (Kind != VK_Array) {
      assert("Cannot convert scalar to non-scalar value.");
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

  Value &operator[](const Value *V) { return *ArrayValue[Hash(V)]; }

  Value &operator[](const Value &V) { return *ArrayValue[Hash(V)]; }

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

  Value &operator--() {
    makeNumber();
    --NumberValue;
    return *this;
  }

  Value operator--(int) {
    makeNumber();
    auto Temp = *this;
    --NumberValue;
    return Temp;
  }

  friend Value operator+(const Value &, const Value &);
  friend Value operator-(const Value &, const Value &);
  friend Value operator*(const Value &, const Value &);
  friend Value operator/(const Value &, const Value &);
  friend bool operator==(const Value &, const Value &);

  operator bool() const {
    switch (Kind) {
    case VK_Null:
      return false;
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
    assert(Kind == VK_Array && "Cannot erase from non-array.");
    ArrayValue.erase(Hash(V));
  }

  void erase(Value &&V) { erase(V); }

  void clear() {
    assert(Kind == VK_Array && "Cannot clear a non-array.");
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

inline bool operator==(const Value &V1, const Value &V2) {
  return V1.toString() == V2.toString();
}

} // namespace cawk
