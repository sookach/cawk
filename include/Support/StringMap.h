#pragma once

#include <algorithm>
#include <cstddef>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <string>
#include <string_view>
#include <utility>

template <typename T> struct BasicEmplaceFn {
  template <typename... A>
  void operator()(auto &Table, std::string_view Key, A &&...Args) {
    Table.emplace_back(Key, T(std::forward<A>(Args)...));
  }
};

template <typename T, typename EmplaceFn = BasicEmplaceFn<T>> class StringMap {
public:
  using key_type = std::string_view;
  using mapped_type = T;
  using value_type = std::pair<key_type, mapped_type>;
  using size_type = std::size_t;
  using const_iterator = std::vector<value_type>::const_iterator;
  using iterator = std::vector<value_type>::iterator;

private:
  std::vector<value_type> Table;

public:
  /// Returns an iterator to the first element of the StringMap.
  iterator begin() { return std::begin(Table); }

  /// Returns an iterator to the first element of the StringMap.
  const_iterator begin() const { return cbegin(); }

  /// Returns an iterator to the first element of the StringMap.
  const_iterator cbegin() const { return std::cbegin(Table); }

  /// Returns an iterator to the element following the last element of the
  /// StringMap.
  iterator end() { return std::end(Table); }

  /// Returns an iterator to the element following the last element of the
  /// StringMap
  const_iterator end() const { return cend(); }

  /// Returns an iterator to the element following the last element of the
  /// StringMap
  const_iterator cend() const { return std::cend(Table); }

  /// Checks if the container has no elements, i.e. whether begin() == end().
  bool empty() const { return begin() == end(); }

  /// Returns the number of elements in the container, i.e.
  /// std::distance(begin(), end()).
  size_type size() const { return std::distance(begin(), end()); }

  /// Returns the maximum number of elements the container is able to hold due
  /// to system or library implementation limitations, i.e.
  /// std::distance(begin(), end()) for the largest container.
  size_type max_size() const { return Table.max_size(); }

  /// Erases all elements from the container. After this call, size() returns
  /// zero. Invalidates any references, pointers, and iterators referring to
  /// contained elements. May also invalidate past-the-end iterators.
  void clear() { Table.clear(); }

  /// Inserts Value into the container, if the container doesn't already
  /// contain an element with an equivalent key.
  std::pair<iterator, bool> insert(const value_type &Value) {
    return try_emplace(Value.first, Value.second);
  }

  /// Inserts Value into the container, if the container doesn't already
  /// contain an element with an equivalent key.
  std::pair<iterator, bool> insert(value_type &&Value) {
    return try_emplace(Value.first, std::move(Value.second));
  }

  /// Inserts elements from range [first, last). If [first, last) is not a valid
  /// range, or first and/or last are iterators into *this, the behavior is
  /// undefined.
  void insert(std::forward_iterator auto First,
              std::forward_iterator auto Last) {
    for (; First != Last; ++First)
      insert(*First);
  }

  /// Inserts elements from initializer list IList.
  void insert(std::initializer_list<value_type> IList) {
    insert(std::cbegin(IList), std::cend(IList));
  }

  /// If a key equivalent to Key already exists in the container, assigns
  /// std::forward<V>(Val) to the mapped_type corresponding to the key Key. If
  /// the key does not exist, inserts the new value as if by insert,
  /// constructing it from value_type(Key, std::forward<V>(Val)).
  template <typename V>
  std::pair<iterator, bool> insert_or_assign(key_type Key, V &&Val) {
    auto Ret = try_emplace(Key, std::forward<V>(Val));
    if (!Ret.second)
      Ret.first->second = std::forward<V>(Val);
    return Ret;
  }

  /// Inserts a new element into the container constructed in-place with the
  /// given args, if there is no element with the key in the container.
  template <typename... A> std::pair<iterator, bool> emplace(A &&...Args) {
    return try_emplace(std::forward<A>(Args)...);
  }

  /// If a key equivalent to Key already exists in the container, does nothing.
  /// Otherwise, inserts a new element into the container with key Key and value
  /// constructed with args.
  template <typename... A>
  std::pair<iterator, bool> try_emplace(key_type Key, A &&...Args) {
    if (auto It = find(Key); It != end())
      return std::make_pair(It, false);

    EmplaceFn()(Table, Key, std::forward<A>(Args)...);
    return std::make_pair(std::prev(end()), true);
  }

  /// Removes the element at It.
  iterator erase(iterator It) { return Table.erase(It); }

  /// Removes the element at It.
  iterator erase(const_iterator It) { return Table.erase(It); }

  /// Removes the elements in the range [First, Last), which must be a valid
  /// range in *this.
  iterator erase(const_iterator First, const_iterator Last) {
    return Table.erase(First, Last);
  }

  /// Removes the element (if one exists) with the key equivalent to key.
  /// Returns true if an element was removed, false otherwise.
  bool erase(key_type Key) {
    auto It = find(Key);
    if (It == end())
      return false;

    std::iter_swap(It, std::prev(end()));
    erase(std::prev(end()));
    return true;
  }

  /// Returns a reference to the mapped value of the element with specified
  /// key. If no such element exists, the program terminates.
  const mapped_type &at(key_type Key) const {
    auto It = find(Key);
    assert(It != end());
    return It->second;
  }

  /// Returns a reference to the value that is mapped to a key equivalent to
  /// Key, performing an insertion if such key does not already exist.
  mapped_type &operator[](key_type Key) {
    return try_emplace(Key).first->second;
  }

  /// Returns the number of elements with key that compares equal to the
  /// specified argument Key, which is either ​0​ or 1 since this container
  /// does not allow duplicates.
  size_type count(key_type Key) const {
    return static_cast<size_type>(contains(Key));
  }

  /// Finds an element with key equivalent to Key.
  iterator find(key_type Key) {
    return begin() +
           std::distance(cbegin(),
                         static_cast<const StringMap &>(*this).find(Key));
  }

  /// Finds an element with key equivalent to Key.
  const_iterator find(key_type Key) const {
    return std::ranges::find(Table, Key, &value_type::first);
  }

  /// Checks if there is an element with key equivalent to Key in the container.
  bool contains(key_type Key) const { return find(Key) != end(); }
};