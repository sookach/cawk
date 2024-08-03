#pragma once

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <variant>

namespace cawk {

class Value {
public:
  enum ValueKind { VK_Scalar, VK_Array };
  enum TypeKind { TK_Null, TK_Number, TK_String, TK_Array };

private:
  ValueKind Kind;
  TypeKind Type = TK_Null;

public:
  Value(TypeKind Type = TK_Null)
      : Kind(Type == TK_Array ? VK_Array : VK_Scalar), Type(Type) {}

  ValueKind getKind() { return Kind; }

  template <TypeKind T> auto getAs() {
    if (Kind == VK_Scalar)
      return static_cast<Scalar *>(this)->getAs<T>();
    return static_cast<Array *>(this)->getAs<T>();
  }

  template <TypeKind T> auto get() {
    if (Kind == VK_Scalar)
      return static_cast<Scalar *>(this)->get<T>();
    return static_cast<Array *>(this)->get<T>();
  }

  bool is(TypeKind T) {
    if (Kind == VK_Scalar)
      return static_cast<Scalar *>(this)->is(T);
    return static_cast<Array *>(this)->is(T);
  }

  void setValue(Scalar S) {
    if (Kind == VK_Scalar)
      static_cast<Scalar *>(this)->setValue(S);
    else
      assert(0);
  }
};

class Scalar : public Value {
private:
  std::variant<double, std::string> Raw;

public:
  Scalar(double Raw) : Value(TK_Number), Raw(Raw) {}
  Scalar(std::string Raw) : Value(TK_String), Raw(Raw) {}

  template <TypeKind T> auto get() {
    if constexpr (T == Number)
      return std::get<double>(Raw);
    else
      return std::get<std::string>(Raw);
  }

  template <TypeKind T> auto getAs() {
    if constexpr (T == TK_Number) {
      if (Type == TK_Null)
        return 0;
      if (Type == TK_Number)
        return std::get<double>(Raw);
      assert(Type == String);
      char *C;
      return std::strtod(std::get<std::string>(Raw).c_str(), &C);
    } else if (Type == TK_String) {
      assert(T == TK_String);
      if (Type == TK_Null)
        return "";
      if (Type == TK_Number)
        return std::to_string(std::get<double>(Raw));
      assert(Type == TK_String);
      char *C;
      return std::get<std::string>(Raw);
    } else {
      return {};
    }
  }

  void setValue(Scalar V) { *this = V; }

  bool is(TypeKind T) { return Type == T; }

  bool isTrue() {
    if (Type == TK_Null)
      return false;

    if (Type == TK_Number)
      return std::get<double>(Raw);

    assert(Type == TK_String);
    return !std::empty(std::get<std::string>(Raw));
  }

  static bool classof(Value *V) { return V->getKind() == VK_Scalar; }
};

class Array : public Value {
  std::unordered_map<std::string, Scalar> Raw;

public:
  template <TypeKind T> std::unordered_map<std::string, Scalar> get() {
    static_assert(T == TK_Array);
    return Raw;
  }

  template <TypeKind T> std::unordered_map<std::string, Scalar> getAs() {
    static_assert(T == TK_Array);
    return Raw;
  }

  Scalar getValue(std::vector<Scalar> Indexes) {
    return Raw[std::ranges::fold_left(Indexes, std::string(), std::plus())];
  }

  void setValue(std::vector<Scalar> Indexes, Scalar S) {
    Raw[std::ranges::fold_left(Indexes, std::string(), std::plus())] = S;
  }

  static bool classof(Value *V) { return V->getKind() == VK_Scalar; }
};
} // namespace cawk