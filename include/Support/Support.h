#pragma once

namespace cawk {

template <auto T> bool isa(auto &&X) { return X.GetKind() == T; }

} // namespace cawk
