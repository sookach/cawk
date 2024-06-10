#pragma once

#include <algorithm>
#include <array>
#include <initializer_list>
#include <iterator>

template <typename T> class Sequence {
public:
  using __self = Sequence;
  using value_type = T;
  using reference = value_type &;
  using const_reference = const value_type &;
  using pointer = value_type *;
  using const_pointer = const value_type *;
  using iterator = pointer;
  using const_iterator = const_pointer;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

private:
  static constexpr size_type N = 8'000 / sizeof(T);
  std::array<T, N> Elems;
  std::size_t Size = 0;

public:
  constexpr Sequence() noexcept = default;

  constexpr Sequence(size_type count, const T &value) { assign(count, value); }

  explicit constexpr Sequence(size_type count) : Sequence(count, {}) {}

  constexpr Sequence(std::input_iterator auto first,
                     std::input_iterator auto last) {
    assign(first, last);
  }

  constexpr Sequence(const Sequence &) = default;

  constexpr Sequence(Sequence &&) = default;

  constexpr Sequence(std::initializer_list<T> init)
      : Sequence(std::cbegin(init), std::cend(init)) {}

  constexpr ~Sequence() = default;

  constexpr Sequence &operator=(const Sequence &) = default;

  constexpr Sequence &operator=(Sequence &&) = default;

  constexpr Sequence &operator=(std::initializer_list<T> init) {
    return *this = Sequence(init);
  }

  constexpr void assign(size_type count, const T &value) {
    std::fill_n(std::begin(Elems), count, value);
    Destroy(std::begin(Elems) + count, std::end(Elems));
    Size = count;
  }

  constexpr void assign(std::input_iterator auto first,
                        std::input_iterator auto last) {
    std::copy(first, last, std::begin(Elems));
    Destroy(begin() + std::distance(first, last), end());
    Size = last - first;
  }

  constexpr void assign(std::initializer_list<T> ilist) {
    assign(std::cbegin(ilist), std::cend(ilist));
    Size = std::size(ilist);
  }

  constexpr reference at(size_type pos) { return Elems.at(pos); }

  constexpr const_reference at(size_type pos) const { return Elems.at(pos); }

  constexpr reference operator[](size_type pos) { return Elems[pos]; }

  constexpr const_reference operator[](size_type pos) const {
    return Elems[pos];
  }

  constexpr reference front() { return Elems.front(); }

  constexpr const_reference front() const { return Elems.front(); }

  constexpr reference back() { return Elems[Size - 1]; }

  constexpr const_reference back() const { return Elems[Size - 1]; }

  constexpr T *data() { return Elems.data(); }

  constexpr const T *data() const { return Elems.data(); }

  constexpr iterator begin() { return std::begin(Elems); }

  constexpr const_iterator begin() const { return std::begin(Elems); }

  constexpr const_iterator cbegin() const noexcept {
    return std::cbegin(Elems);
  }

  constexpr iterator end() { return begin() + Size; }

  constexpr const_iterator end() const { return end() + Size; }

  constexpr const_iterator cend() const noexcept { return cbegin() + Size; }

  constexpr reverse_iterator rbegin() { return std::reverse_iterator(end()); }

  constexpr const_reverse_iterator rbegin() const {
    return std::reverse_iterator(end());
  }

  constexpr const_reverse_iterator crbegin() const noexcept {
    return std::reverse_iterator(cend());
  }

  constexpr reverse_iterator rend() { return std::reverse_iterator(begin()); }

  constexpr const_reverse_iterator rend() const {
    return std::reverse_iterator(begin());
  }

  constexpr const_reverse_iterator crend() const noexcept {
    return std::reverse_iterator(cbegin());
  }

  constexpr bool empty() const noexcept { return std::empty(Elems); }

  constexpr size_type size() const noexcept { return Size; }

  constexpr size_type max_size() const noexcept { return N; }

  constexpr void reserve() const {}

  constexpr size_type capacity() const noexcept { return N; }

  constexpr void shrink_to_fit() const {}

  constexpr void clear() noexcept {
    Destroy();
    Size = 0;
  }

  constexpr iterator insert(const_iterator pos, const T &value) {
    std::move_backward(pos, end(), pos + 1);
    *pos = value;
    ++Size;
    return pos;
  }

  constexpr iterator insert(const_iterator pos, T &&value) {
    std::move_backward(pos, end(), pos + 1);
    *pos = std::move(value);
    ++Size;
    return pos;
  }

  constexpr iterator insert(const_iterator pos, size_type count,
                            const T &value) {
    std::move_backward(pos, end(), pos + count);
    std::fill_n(pos, count, value);
    Size += count;
    return pos;
  }

  template <std::input_iterator I>
  constexpr iterator insert(const_iterator pos, I first, I last) {
    std::move_backward(pos, end(), pos + std::distance(first, last));
    std::copy(first, last, pos);
    Size += std::distance(first, last);
    return pos;
  }

  constexpr iterator insert(const_iterator pos,
                            std::initializer_list<T> ilist) {
    return insert(pos, std::cbegin(ilist), std::cend(ilist));
  }

  template <typename... Args>
  constexpr iterator emplace(const_iterator pos, Args &&...args) {
    std::move_backward(pos, end(), pos + 1);
    *pos = T(std::forward<Args>(args)...);
    ++Size;
    return pos;
  }

  constexpr iterator erase(const_iterator pos) {
    Destroy(pos);
    std::move_backward(pos + 1, end(), pos);
    ++Size;
    return pos;
  }

  constexpr iterator erase(const_iterator first, const_iterator last) {
    Destroy(first, last);
    std::move_backward(last, end(), first);
    Size -= last - first;
    return first;
  }

  constexpr void push_back(const T &value) { Elems[Size++] = value; }

  constexpr void push_back(T &&value) { Elems[Size++] = std::move(value); }

  template <typename... Args> constexpr reference emplace_back(Args &&...args) {
    return Elems[Size++] = T(std::forward<Args>(args)...);
  }

  constexpr void pop_back() {
    Destroy(end() - 1);
    --Size;
  }

  void resize(size_type count, const value_type &value = {}) {
    if (count > Size)
      insert(end(), count, value);
    else {
      Destroy(begin() + count, end());
      Size = count;
    }
  }

private:
  constexpr void Destroy() { Destroy(begin(), end()); }

  constexpr void Destroy(iterator Pos, size_type Count = 0) {
    Destroy(Pos, Pos + Count);
  }

  constexpr void Destroy(iterator First, iterator Last) {
    std::for_each(First, Last, [](auto &&X) constexpr {
      if constexpr (!std::is_pointer_v<T>)
        ~X.T();
    });
  }
};
