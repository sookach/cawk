#include "Exec/Value.h"

#include "AST/Expr.h"

using namespace cawk;

Value::Value(LambdaExpr *L)
    : Type(FunctionTy), FunctionValue(L->getParams(), L->getBody(), true) {}