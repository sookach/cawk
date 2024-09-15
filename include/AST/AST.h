#pragma once

#include "AST/Decl.h"
#include "AST/Expr.h"
#include "AST/Stmt.h"

#include <utility>

namespace cawk {

/// @brief Represents the result of a parse operation.
/// @tparam T The type of the result (Decl, Stmt, Expr).
template <typename T> struct ASTResult : private std::pair<T *, bool> {
  ASTResult(bool Invalid = false) : std::pair<T *, bool>(nullptr, Invalid) {}
  ASTResult(T *Ptr) : std::pair<T *, bool>(Ptr, true) {}
  T *get() { return this->first; }
  template <typename Ty> Ty *getAs() { return static_cast<Ty *>(get()); }
  bool isValid() { return this->second; }
  ASTResult &operator=(T *RHS) {
    this->first = RHS;
    this->second = true;
    return *this;
  }
};

using DeclResult = ASTResult<Decl>;
using StmtResult = ASTResult<Stmt>;
using ExprResult = ASTResult<Expr>;

} // namespace cawk
