#pragma once

#include <algorithm>
#include <string>
#include <utility>

template <typename T> class SymbolTable {
public:
  using Element = std::pair<std::string, T>;

private:
  std::vector<Element> Table;

  decltype(Table)::iterator lookup(std::string_view Key) const {
    return std::ranges::find(Table, Key,
                             [](const Element &E) { return E.first; });
  }

public:
  std::optional<T> get(std::string_view Key) const {
    auto I = lookup(Key);
    return I == std::cend(Table) ? std::nullopt : std::make_optional(I->second);
  }

  void set(std::string_view Key, T Value) {
    auto I = lookup(Key);
    if (I == std::cend(Table))
      Table.emplace_back(Key.data(), Value);
    else
      I->second = Value;
  }
};
