#pragma once

#include <algorithm>
#include <string>
#include <utility>

template <typename Derived, typename T> class SymbolTable {
public:
  using Element = std::pair<std::string, T>;

  bool contains(std::string_view Key) const {
    return static_cast<const Derived *>(this)->contains(Key);
  }

  T &get(std::string_view Key) const {
    return static_cast<const Derived *>(this)->get(Key);
  }

  void set(std::string_view Key, T Value) {
    return static_cast<Derived *>(this)->set(Key, Value);
  }
};

template <typename T>
class BasicSymbolTable : SymbolTable<BasicSymbolTable<T>, T> {
public:
  using Element = std::pair<std::string, T>;

private:
  std::vector<Element> Table;

  auto lookup(std::string_view Key) const {
    return std::ranges::find(Table, Key,
                             [](const Element &E) { return E.first; });
  }

public:
  bool contains(std::string_view Key) const {
    return lookup(Key) != std::cend(Table);
  }

  T &get(std::string_view Key) const {
    assert(contains(Key) && "Key not in Symbol table");
    return const_cast<T &>(lookup(Key)->second);
  }

  void set(std::string_view Key, T Value) {
    if (contains(Key))
      get(Key) = Value;
    else
      Table.emplace_back(std::string(Key), Value);
  }
};

template <typename T>
class FunctionSymbolTable : SymbolTable<FunctionSymbolTable<T>, T> {
public:
  using Element = std::pair<std::string, T>;

private:
  std::vector<Element> Table;

  auto lookup(std::string_view Key) const {
    return std::ranges::find(Table, Key,
                             [](const Element &E) { return E.first; });
  }

public:
  bool contains(std::string_view Key) const {
    return lookup(Key) != std::cend(Table);
  }

  T &get(std::string_view Key) const {
    assert(contains(Key) && "Key not in Symbol table");
    return const_cast<T &>(lookup(Key)->second);
  }

  void set(std::string_view Key, T Value) {
    if (contains(Key))
      get(Key) =
          T::Create(Value.getIdentifier(), Value.getParams(), Value.getBody());
    else
      Table.emplace_back(
          std::string(Key),
          T::Create(Value.getIdentifier(), Value.getParams(), Value.getBody()));
  }
};