#pragma once

#include <string>
#include <unordered_map>
#include <variant>

class Data;

class Primitive final {
  std::variant<double, std::string> Value;
};

using Array = std::unordered_map<Data *, Data *>;

class Data final {
public:
  enum TypeKind { TK_Primitive, TK_Array };

private:
  std::variant<Primitive, Array> Value;
  const TypeKind Type;

public:
  TypeKind GetType() const { return Type; }
};
