#ifndef TUP20_SIZE_SORTED_TUPLE_HPP_INCLUDE_GUARD
#define TUP20_SIZE_SORTED_TUPLE_HPP_INCLUDE_GUARD

#include "impl/macros.hpp"

#include "tuple.hpp"
#include "permute.hpp"

#include <numeric>
#include <algorithm>

namespace tup20 {

/**
 * Creates a comparison function that compares the image of its inputs
 * under f. cmp_by(f,less)(x,y) => less(f(x),f(y))
 */
static constexpr auto cmp_by(auto f, auto less = std::less<>{}) {
  return [ f    = std::move(f),
           less = std::move(less) ] TUP20_FN(less(f(_0), f(_1)));
}

template<auto attribute, class less, class... Ts>
inline constexpr auto attribute_sorting_indices = [] {
  constexpr auto       N{sizeof...(Ts)};
  constexpr std::array attrs{attribute.template operator()<Ts>()...};

  permutation<N> indices;
  std::iota(std::begin(indices), std::end(indices), 0u);
  std::sort(std::begin(indices),
            std::end(indices),
            cmp_by([=] TUP20_FN(attrs[_]), less {}));
  return indices;
}();

template<auto attribute, class less, class... Ts>
inline constexpr auto attribute_sorting_permutation =
    invert(attribute_sorting_indices<attribute, less, Ts...>);

template<class... Ts>
inline constexpr auto size_descending_permutation =
    attribute_sorting_permutation<[]<class T>() { return sizeof(T); },
                                  std::greater<>,
                                  Ts...>;

template<class... Ts>
struct size_sorted_tuple
    : storage_permuted_tuple<size_descending_permutation<Ts...>, Ts...> {
  using base =
      storage_permuted_tuple<size_descending_permutation<Ts...>, Ts...>;
  using base::base;
};
template<class... Ts>
size_sorted_tuple(Ts...) -> size_sorted_tuple<Ts...>;
} // namespace tup20

template<class... Ts>
struct std::tuple_size<tup20::size_sorted_tuple<Ts...>>
    : std::integral_constant<std::size_t, sizeof...(Ts)> {};

template<std::size_t N, class... Ts>
struct std::tuple_element<N, tup20::size_sorted_tuple<Ts...>> {
  using type = decltype(tup20::get_n<N>(
      std::declval<tup20::size_sorted_tuple<Ts...>>()));
};

#endif // TUP20_SIZE_SORTED_TUPLE_HPP_INCLUDE_GUARD
