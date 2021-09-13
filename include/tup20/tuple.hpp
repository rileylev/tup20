#ifndef TUPLE_INCLUDE_GUARD_HPP
#define TUPLE_INCLUDE_GUARD_HPP

#include "impl/macros.hpp"

#include <cstdint>
#include <utility>
#include <type_traits>
#include <compare>
#include <tuple>

#ifndef TUP20_USE_TYPE_PACK_ELEMENT
#  define TUP20_USE_TYPE_PACK_ELEMENT __has_builtin(__type_pack_element)
#endif

namespace tup20 {
namespace { namespace niebloid_impl {
using std::get;

template<class T>
struct get_t {
  constexpr auto operator()(auto&& tup) const
      TUP20_ARROW(get<T>(TUP20_FWD(tup)))
};

template<auto N>
struct get_n {
  constexpr auto operator()(auto&& tup) const
      TUP20_ARROW(get<N>(TUP20_FWD(tup)))
};
}} // namespace ::niebloid_impl

template<class T>
inline constexpr niebloid_impl::get_t<T> get_t{};
template<auto N>
inline constexpr niebloid_impl::get_n<N> get_n{};

namespace impl {
// It doesn't compress a tuple with two of the same empty types
template<std::size_t index, class Type>
struct box {
  [[no_unique_address]] Type elt;
};

struct tuple_friends {
 private:
#define TUP20_DEFINE_GET_HELPERS(crf, move_)                              \
  template<auto N, class T>                                               \
  static constexpr auto get_n_(box<N, T> crf tup)                         \
      TUP20_ARROW(move_(tup.elt))                                         \
  template<class T, auto N>                                               \
  static constexpr auto get_t_(box<N, T> crf tup)                         \
      TUP20_ARROW(move_(tup.elt))

  TUP20_DEFINE_GET_HELPERS(&, )
  TUP20_DEFINE_GET_HELPERS(const&, )
  TUP20_DEFINE_GET_HELPERS(&&, std::move)
  TUP20_DEFINE_GET_HELPERS(const&&, std::move)
#undef TUP20_DEFINE_GET_HELPERS

 public:
  template<auto N>
  friend constexpr auto get(auto&& tup) TUP20_ARROW(
#if TUP20_USE_TYPE_PACK_ELEMENT
      get_n_<N,
             typename std::remove_cvref_t<decltype(tup)>::template nth_t<N>>(
          TUP20_FWD(tup))
#else
      get_n_<N>(TUP20_FWD(tup))
#endif
  )
  template<class T>
  friend constexpr auto get(auto&& tup)
      TUP20_ARROW(get_t_<T>(TUP20_FWD(tup)))
};

template<class seq, class... Ts>
struct tuple_impl;
template<auto... Is, class... Ts>
struct tuple_impl<std::index_sequence<Is...>, Ts...>
    : tuple_friends, box<Is, Ts>... {
  tuple_impl() = default;

  template<class... Args>
  requires((std::is_constructible_v<Ts, Args&&> and ...))    //
      explicit(!(std::is_convertible_v<Ts, Args&&> and ...)) //
      constexpr tuple_impl(Args&&... args) noexcept(
          (std::is_nothrow_constructible_v<Ts, Args&&> and ...))
      : box<Is, Ts>{TUP20_FWD(args)}... {}

  template<class Tup>
  explicit(!(std::is_convertible_v<
                 Ts,
                 decltype(get_n<Is>(std::declval<Tup>()))> and ...)) //
      constexpr tuple_impl(Tup&& other) noexcept(
          (std::is_nothrow_constructible_v<
               Ts,
               decltype(get_n<Is>(TUP20_FWD(other)))> && ...)) //
      requires((sizeof...(Ts) == std::tuple_size<std::remove_cvref_t<Tup>>{})
               and (std::is_constructible_v<
                        Ts,
                        decltype(get_n<Is>(TUP20_FWD(other)))> and ...))
      : box<Is, Ts>{get_n<Is>(TUP20_FWD(other))}... {}

#if TUP20_USE_TYPE_PACK_ELEMENT
  template<auto N>
  using nth_t = __type_pack_element<N, Ts...>;
#endif
};
} // namespace impl

/**
 * A tuple that is trivially copyable if its components are
 */
template<class... Ts>
struct tuple : impl::tuple_impl<std::index_sequence_for<Ts...>, Ts...> {
  using base = impl::tuple_impl<std::index_sequence_for<Ts...>, Ts...>;
  using base::base;
};

template<class... Ts>
tuple(Ts...) -> tuple<Ts...>;

} // namespace tup20

// specialization for std::tuple_size, element
template<class... Ts>
struct std::tuple_size<tup20::tuple<Ts...>>
    : std::integral_constant<std::size_t, sizeof...(Ts)> {};
template<std::size_t N, class... Ts>
struct std::tuple_element<N, tup20::tuple<Ts...>> {
  using type = decltype(get<N>(std::declval<tuple<Ts...>>()));
};

namespace tup20 {
namespace impl {
template<auto size, class X, class Y>
concept same_tuple_size =
    size == std::tuple_size<std::remove_cvref_t<X>>{}
    and size == std::tuple_size<std::remove_cvref_t<Y>>{};

template<class X, class Y, auto... Is>
requires(same_tuple_size<sizeof...(Is), X, Y>) //
    constexpr static auto equal_(X const& x,
                                 Y const& y,
                                 std::index_sequence<Is...>)
        TUP20_ARROW(((get_n<Is>(x) == get_n<Is>(y)) and ...))

template<class X, class Y, auto... Is>
requires(same_tuple_size<sizeof...(Is), X, Y>) //
    constexpr static auto spaceship_(X const& x,
                                     Y const& y,
                                     std::index_sequence<Is...>) //
    noexcept((noexcept(get_n<Is>(x) <=> get_n<Is>(y)) and ...))
        -> std::common_comparison_category_t<
            decltype(get_n<Is>(x) <=> get_n<Is>(y))...> {
  using Ord = decltype(x <=> y);
  Ord ord   = Ord::equivalent;
  static_cast<void>(((ord = (get_n<Is>(x) <=> get_n<Is>(y)),
                      std::is_eq(ord)) // while (std::is_eq(ord))
                     and ...));
  return ord;
}
} // namespace impl

template<class X, class Y>
constexpr auto operator==(X const& x, Y const& y) TUP20_ARROW(
    impl::equal_(x, y, std::make_index_sequence<std::tuple_size<X>::value>{}))
template<class X, class Y>
constexpr auto operator<=>(X const& x, Y const& y)
    TUP20_ARROW(impl::spaceship_(
        x,
        y,
        std::make_index_sequence<std::tuple_size<X>::value>{}))
} // namespace tup20
#endif // TUPLE_INCLUDE_GUARD_HPP
