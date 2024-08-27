#pragma once

#include <string_view>
#include <utility>

namespace cawk {
class SourceRange : public std::pair<std::string_view::const_iterator,
                                     std::string_view::const_iterator> {
  bool Valid = false;

public:
  SourceRange() = default;
  SourceRange(std::string_view::const_iterator Begin,
              std::string_view::const_iterator End)
      : std::pair<std::string_view::const_iterator,
                  std::string_view::const_iterator>(Begin, End),
        Valid(true) {}

  std::string_view::const_iterator getBegin() const { return this->first; }
  std::string_view::const_iterator getEnd() const { return this->second; }
  bool isValid() const { return Valid; }
};
} // namespace cawk