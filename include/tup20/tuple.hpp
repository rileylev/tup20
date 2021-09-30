#ifndef TUPLE_INCLUDE_GUARD_HPP
#define TUPLE_INCLUDE_GUARD_HPP

#include "impl/macros.hpp"

#include <cstdint>
#include <utility>
#include <type_traits>
#include <compare>

/**
 * Clang has a builtin __type_pack_element which can pick the nth element
 * from a parameter pack. When it's available, get<n> and tuple_element_t
 * should use it.
 * */
#ifndef TUP20_USE_TYPE_PACK_ELEMENT
#  define TUP20_USE_TYPE_PACK_ELEMENT __has_builtin(__type_pack_element)
#endif

#ifndef TUP20_FWD_DECLARE_STD_TUPLE_UTILITIES
#  define TUP20_FWD_DECLARE_STD_TUPLE_UTILITIES true
#endif
#if TUP20_FWD_DECLARE_STD_TUPLE_UTILITIES
// risky to forward declare these? but it seems like the closest way to say
// "do this if we #include <tuple> later"
template<class T>
struct std::tuple_size {};
template<std::size_t N, class T>
struct std::tuple_element {};
#else
#  include <tuple>
#endif

namespace tup20 {

namespace impl {
template<class Tup>
concept has_size = requires {
  std::remove_cvref_t<Tup>::size;
};
template<auto N, class Tup>
concept has_nth_t = requires {
  typename Tup::template nth_t<N>;
};
} // namespace impl
template<class Tup>
constexpr auto size = [] {
  using Tup_ = std::remove_cvref_t<Tup>;
  if constexpr(impl::has_size<Tup_>) return Tup_::size;
  else return std::tuple_size<Tup_>::value;
}();

template<auto N, class Tup>
using nth_t = decltype([] {
  using Tup_ = std::remove_cvref_t<Tup>;
  if constexpr(impl::has_nth_t<N, Tup_>)
    return std::declval<Tup_::template nth_t<N>>();
  else return get_n<N>(std::declval<Tup_>());
}());

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
template<class>
constexpr auto delay(auto x) TUP20_RET(x);

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
#if TUP20_USE_TYPE_PACK_ELEMENT
#  define TUP20_COMMA_NTH_TYPE(tup, N)                                    \
    , typename std::remove_cvref_t<decltype(tup)>::template nth_t<N>
#else
#  define TUP20_COMMA_NTH_TYPE(tup, N)
#endif

  template<auto N, class Tup>
  requires(N >= 0 and N < tup20::size<Tup>) //
      static constexpr auto get(Tup&& tup)
          TUP20_ARROW(tuple_friends::get_n_<N TUP20_COMMA_NTH_TYPE(tup, N)>(
              TUP20_FWD(tup)));
  template<auto N, class Tup>
  static constexpr auto get(Tup&&) {
    static_assert(
        N >= 0 and N < tup20::size<Tup>,
        "Bad index for get<N>(tuple)"
        "N must be nonnegative. N must be smalle than the tuple's size.");
  }

  template<class T, class Tup>
  requires(true) // prioritize
      static constexpr auto get(Tup&& tup)
          TUP20_ARROW(tuple_friends::get_t_<T>(TUP20_FWD(tup)))
  template<class T, class Tup>
  static constexpr auto get(Tup&&) {
    static_assert(
        delay<T>(false),
        "get<type>(tuple) only works if type is in tuple exactly once");
  }

  template<class T>
  friend constexpr auto get(auto&& tup)
      TUP20_ARROW(tuple_friends::get<T>(TUP20_FWD(tup)))

  template<auto N>
  friend constexpr auto get(auto&& tup)
      TUP20_ARROW(tuple_friends::get<N>(TUP20_FWD(tup)))
};

template<class Self, auto N, class... Ts>
using nth_t_impl =
#if TUP20_USE_TYPE_PACK_ELEMENT
   __type_pack_element<N, Ts...>;
#else
   decltype(get_n<N>(std::declval<Self>()));
#endif

template<class seq, class... Ts>
struct tuple_impl;
template<auto... Is, class... Ts>
struct tuple_impl<std::index_sequence<Is...>, Ts...>
    : tuple_friends, box<Is, Ts>... {
  static constexpr auto size = sizeof...(Ts);
  template<auto N>
  using nth_t = nth_t_impl<tuple_impl,N, Ts...>;
  tuple_impl()               = default;

  template<class... Args>
  requires((std::is_constructible_v<Ts, Args&&> and ...))         //
      TUP20_IMPLICIT((std::is_convertible_v<Ts, Args&&> and ...)) //
  constexpr tuple_impl(Args&&... args) noexcept(
      (std::is_nothrow_constructible_v<Ts, Args&&> and ...))
      : box<Is, Ts>{TUP20_FWD(args)}... {}

  template<class Tup>
  TUP20_IMPLICIT((std::is_convertible_v<
                      Ts,
                      decltype(get_n<Is>(std::declval<Tup>()))> and ...)) //
  constexpr tuple_impl(Tup&& other) noexcept(
      (std::is_nothrow_constructible_v<
           Ts,
           decltype(get_n<Is>(TUP20_FWD(other)))> and ...)) //
      requires((sizeof...(Ts) == tup20::size<std::remove_cvref_t<Tup>>)and(
          std::is_constructible_v<Ts,
                                  decltype(get_n<Is>(TUP20_FWD(other)))> //
              and...))
      : box<Is, Ts>{get_n<Is>(TUP20_FWD(other))}... {}
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

template<class Tup>
requires(requires { Tup::size; }) struct std::tuple_size<Tup>
    : std::integral_constant<std::size_t, Tup::size> {
};
template<std::size_t N, class Tup>
requires(requires {
  typename Tup::template nth_t<N>;
}) struct std::tuple_element<N, Tup> {
  using type = typename Tup::template nth_t<N>;
};

namespace tup20 {
namespace impl {
template<auto size, class X, class Y>
concept same_tuple_size =
    size == tup20::size<std::remove_cvref_t<X>>and size
    == tup20::size<std::remove_cvref_t<Y>>;

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
  (void)((ord = (get_n<Is>(x) <=> get_n<Is>(y)),
          std::is_eq(ord)) // while (std::is_eq(ord))
         and ...);
  return ord;
}
} // namespace impl

template<class X, class Y>
constexpr auto operator==(X const& x, Y const& y) TUP20_ARROW(
    impl::equal_(x, y, std::make_index_sequence<tup20::size<X>>{}))

template<class X, class Y>
constexpr auto operator<=>(X const& x, Y const& y) TUP20_ARROW(
    impl::spaceship_(x, y, std::make_index_sequence<tup20::size<X>>{}))
} // namespace tup20
#endif // TUPLE_INCLUDE_GUARD_HPP
