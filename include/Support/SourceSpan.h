#pragma once

#include <fstream>
#include <iterator>
#include <string>
#include <string_view>
#include <unordered_map>

class SourceSpan {
  std::unordered_map<std::string_view, std::string> Buffers;

public:
  void AddSource(std::string_view Name) { Buffers[Name] = GetFileBytes(Name); }

  std::string_view GetSource(std::string_view Name) const {
    return const_cast<SourceSpan *>(this)->Buffers[Name];
  }

  void RemoveSource(std::string_view Name) { Buffers.erase(Name); }

private:
  std::string GetFileBytes(std::string_view Name) {
    std::ifstream File(Name.data());
    std::string s((std::istreambuf_iterator<char>(File)),
                  std::istreambuf_iterator<char>());
    return s;
  }
};
