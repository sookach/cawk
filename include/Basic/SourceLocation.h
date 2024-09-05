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
  SourceRange(std::string_view Source)
      : std::pair<std::string_view::const_iterator,
                  std::string_view::const_iterator>(std::cbegin(Source),
                                                    std::cend(Source)),
        Valid(true) {}

  std::string_view::const_iterator begin() const { return this->first; }
  std::string_view::const_iterator end() const { return this->second; }
  bool isValid() const { return Valid; }
};
} // namespace cawk