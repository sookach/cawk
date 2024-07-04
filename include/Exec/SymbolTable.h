#pragma once

#include <algorithm>
#include <string>
#include <utility>

template <typename T> class SymbolTable {
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
    return const_cast<T&>(lookup(Key)->second);
  }

  void set(std::string_view Key, T Value) {
    if (contains(Key))
      get(Key) = Value;
    else
      Table.emplace_back(std::string(Key), Value);
  }
};
