#pragma once

#include "ast.h"

#include <unordered_map>

namespace cawk {
class scope {
  scope *parent_{};
  std::unordered_map<std::string, decl *> symbols_{};

public:
  scope(scope *parent = nullptr) : parent_{parent} {}

  bool insert(decl *d) { return symbols_.emplace(d->iden_.lexeme_, d).second; }

  constexpr decl *lookup(std::string_view name) const noexcept {
    for (auto s{this}; s != nullptr; s = s->get_parent())
      if (s->symbols_.contains(name.data()))
        return s->symbols_.at(name.data());
    return nullptr;
  }

  constexpr scope *get_parent() const noexcept { return parent_; }
};
} // namespace cawk