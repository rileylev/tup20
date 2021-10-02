#ifndef TUPLE_INCLUDE_GUARD_HPP
#define TUPLE_INCLUDE_GUARD_HPP

#include "impl/macros.hpp"

#include <cstdint>
#include <utility>
#include <type_traits>
#include <compare>
#include <functional> // std::invoke for implementing apply

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

namespace {
template<class Tup>
concept has_size_member = requires {
  std::remove_cvref_t<Tup>::size;
};
template<auto N, class Tup>
concept has_nth_t_member = requires {
  typename Tup::template nth_t<N>;
};
} // namespace
/**
 * Get the size of Tup. This prefers a static member called size if it
 * exists, but falls back to tuple_size.
 *
 * TODO: is this too permissive/easy to accidentally treat somthing as a
 * tuple that shouldn't be?
 */
template<class Tup>
constexpr auto size = [] {
  using Tup_ = std::remove_cvref_t<Tup>;
  if constexpr(has_size_member<Tup_>) return Tup_::size;
  else return std::tuple_size<Tup_>::value;
}();

namespace {
template<class From, class To>
using copy_const =
    std::conditional_t<std::is_const_v<From>, std::add_const_t<To>, To>;

template<class From, class To>
using copy_volatile =
    std::conditional_t<std::is_volatile_v<From>, std::add_volatile_t<To>, To>;

template<class From, class To>
using copy_cv = copy_volatile<From, copy_const<From, To>>;
} // namespace

/**
 * Get the type of the Nth element of Tup. This prefers a member type alias
 * called nth_t, but falls back to get<N>.
 *
 * We prefer the member type alias because it is easier to call
 * __type_pack_element from inside the class.
 */
// TODO: fix value category+qualifiers
template<auto N, class Tup>
using nth_t = decltype([] {
  using Tup_ = std::remove_reference_t<Tup>;
  if constexpr(has_nth_t_member<N, Tup_>)
    return std::declval<copy_cv<Tup_, typename Tup_::template nth_t<N>>>();
  else return std::tuple_element<N, Tup_>::type;
  // going by get has the wrong value categories
  // maybe they can be deduced by reference collapsing but that seems like a mess
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
/**
 * Returns its input x unchanged. This adds a vacuous depnedency on a
 * template parameter, which delays evaluation. Useful when you want to
 * static_assert(false) whenever a specific template is expanded
 */
template<class>
constexpr auto delay(auto x) TUP20_RET(x);

/**
 * The nth wrapper of type Type for implementing the inheritence trick.
 *
 * tup20::tuple is implemented with the inheritence trick.
 * tup20::tuple<T0,T1,...Tn> is implemented by inheriting from
 * box<0,T0>, box<1,T1>...box<n,Tn>. This creates n member variables called
 * elt to store each type. It lets us implement get via overloads on box.
 * (See tuple_friends::{get_n_,get_t_})
 */
template<std::size_t index, class Type>
struct box {
  [[no_unique_address]] Type elt;
};

/**
 * Use the inheritence trick to extract the Nth type.
 *
 * get,get_n won't work here because they add reference qualifiers.
 */
template<auto N, class T>
constexpr T get_nth_t(box<N, T>);

/**
 * To define get as a hidden friend for tuple. Note: this struct is not a
 * template. So we only define these friends once and reuse them for each
 * instantiation of tuple
 */
struct tuple_friends {

 private:
#define TUP20_DEFINE_GET_HELPERS(constref, move_)                         \
  /* get_n_<0> uses overload resolution to find the appropriate T , cast  \
   * the tuple to box<0,T0>, and then access its elt. */                  \
  template<auto N, class T>                                               \
  static constexpr auto get_n_(box<N, T> constref tup)                    \
      TUP20_ARROW(move_(tup.elt))                                         \
  /* This is the same idea, but for accessing by type */                  \
  template<class T, auto N>                                               \
  static constexpr auto get_t_(box<N, T> constref tup)                    \
      TUP20_ARROW(move_(tup.elt))

  TUP20_DEFINE_GET_HELPERS(&, )
  TUP20_DEFINE_GET_HELPERS(const&, )
  TUP20_DEFINE_GET_HELPERS(&&, std::move)
  TUP20_DEFINE_GET_HELPERS(const&&, std::move)
#undef TUP20_DEFINE_GET_HELPERS

#if TUP20_USE_TYPE_PACK_ELEMENT
  // if we have __type_pack_element, we can grab the type before calling
  // get. So we can pass it to get (less searching in overload resolution?)
#  define TUP20_COMMA_NTH_TYPE(tup, N)                                    \
    , typename std::remove_reference_t<decltype(tup)>::template nth_t<N>
#else
#  define TUP20_COMMA_NTH_TYPE(tup, N)
#endif

  // implement get with helpful error messages. We need overload resolution
  // to conditionally error with the helpful message. I think it makes more
  // sense to search through static methods for this, not friend functions/adl
  // TODO: does it really make sense to do it this way
  template<auto N, class Tup>
  requires(N >= 0 and N < tup20::size<Tup>) //
      static constexpr auto get(Tup&& tup)
          TUP20_ARROW(tuple_friends::get_n_<N TUP20_COMMA_NTH_TYPE(tup, N)>(
              TUP20_FWD(tup)));
  template<auto N, class Tup>
  static constexpr auto get(Tup&&) {
    static_assert(0 <= N, "Bad get<N>(tuple) index! N can't be negative");
    static_assert(N < tup20::size<Tup>,
                  "Bad get<N>(tuple) index!"
                  "N must be smaller than the tuple's size.");
  }

  template<class T, class Tup>
  requires(true) // prioritize
      static constexpr auto get(Tup&& tup)
          TUP20_ARROW(tuple_friends::get_t_<T>(TUP20_FWD(tup)))
  template<class T, class Tup>
  static constexpr auto get(Tup&&) {
    constexpr auto T_in_Tup = []<auto... I>(std::index_sequence<I...>) {
      // return (std::is_same_v<tup20::nth_t<I,Tup>, T>or ...);
      // ^^^^ this line is tripping up GCC for some reason?
      constexpr auto is_nth = []<auto N>() {
        return std::is_same_v<nth_t<N, Tup>, T>;
      };
      return (is_nth.template operator()<I>() or ...);
    }
    (std::make_index_sequence<size<Tup>>{});
    if constexpr(T_in_Tup)
      static_assert(delay<T>(false),
                    "Bad type for get<type>(tuple)! The call is "
                    "ambiguous: type is not unique in tuple.");
    else
      static_assert(delay<T>(false),
                    "Bad type for get<type>(tuple)! There is no element "
                    "in tuple with that type.");
  }

 public:
  /**
   * Get the unique element of type T from tup. If there is no such element,
   * or there are multiple elements of type T, that is a compile error.
   */
  template<class T>
  friend constexpr auto get(auto&& tup)
      TUP20_ARROW(tuple_friends::get<T>(TUP20_FWD(tup)))

  /**
   * Get the Nth element of tup. If no such element exists, compile error.
   */
  template<auto N>
  friend constexpr auto get(auto&& tup)
      TUP20_ARROW(tuple_friends::get<N>(TUP20_FWD(tup)))
};

template<class seq, class... Ts>
struct tuple_impl;
template<auto... Is, class... Ts>
struct tuple_impl<std::index_sequence<Is...>, Ts...>
    : tuple_friends, box<Is, Ts>... {

  static constexpr auto size = sizeof...(Ts);

  template<auto N>
  using nth_t =
#if TUP20_USE_TYPE_PACK_ELEMENT
      __type_pack_element<N, Ts...>;
#else
      // should I pull this out for easier testing/compiler checking?
      decltype(impl::get_nth_t<N>(std::declval<tuple_impl>()));
#endif

  tuple_impl() = default;

  template<class... Args>
  requires((std::is_constructible_v<Ts, Args&&> and ...))         //
      TUP20_IMPLICIT((std::is_convertible_v<Ts, Args&&> and ...)) //
  constexpr tuple_impl(Args&&... args) noexcept(
      (std::is_nothrow_constructible_v<Ts, Args&&> and ...))
      : box<Is, Ts>{TUP20_FWD(args)}... {}

  /**
   * Memberwise converting constructor
   */
  template<class Tup>
      TUP20_IMPLICIT(
          (std::is_convertible_v<
               Ts,
               decltype(get_n<Is>(std::declval<Tup>()))> and ...)) //
      constexpr tuple_impl(Tup&& other) noexcept(
          (std::is_nothrow_constructible_v<
               Ts,
               decltype(get_n<Is>(TUP20_FWD(other)))> and ...)) //
      requires(sizeof...(Ts) == tup20::size<std::remove_cvref_t<Tup>>)
      and (std::is_constructible_v<
               Ts,
               decltype(get_n<Is>(TUP20_FWD(other)))> and ...)
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

template<class X, class Y, auto... I>
static constexpr auto
    equal_(X const& x, Y const& y, std::index_sequence<I...>)
        TUP20_ARROW(((get_n<I>(x) == get_n<I>(y)) and ...))
template<class X, class Y>
requires(size<X> == size<Y>) constexpr auto
    operator==(X const& x, Y const& y)
        TUP20_ARROW(equal_(x, y, std::make_index_sequence<tup20::size<X>>{}))

template<class X, class Y, auto... I>
static constexpr auto
    spaceship_(X const& x, Y const& y, std::index_sequence<I...>) noexcept(
        (noexcept(get_n<I>(x) <=> get_n<I>(y)) and ...))
        -> std::common_comparison_category_t<
            decltype(get_n<I>(x) <=> get_n<I>(y))...> {
  using Ord = decltype(x <=> y);
  Ord ord   = Ord::equivalent;
  (void)((ord = (get_n<I>(x) <=> get_n<I>(y)),
          std::is_eq(ord)) // while (std::is_eq(ord))
         and ...);
  return ord;
}
template<class X, class Y>
requires(size<X> == size<Y>) constexpr auto
    operator<=>(X const& x, Y const& y) TUP20_ARROW(
        spaceship_(x, y, std::make_index_sequence<tup20::size<X>>{}))

constexpr auto make_tuple = [](auto... x) TUP20_ARROW(tup20::tuple{x...});

template<auto... I, auto... J>
static constexpr auto
    cat2_(std::index_sequence<I...>,
          std::index_sequence<J...>,
          auto const& x,
          auto const& y,
          auto        make_tup)
        TUP20_ARROW(make_tup(get_n<I>(x)..., get_n<J>(y)...))

template<class X, class Y, class MakeTup = decltype(make_tuple)>
static constexpr auto cat2(X const& x, Y const& y, MakeTup make_tup = {})
    TUP20_ARROW(cat2_(std::make_index_sequence<size<X>>{},
                      std::make_index_sequence<size<Y>>{},
                      x,
                      y,
                      make_tup))

namespace {
/**
 * for folding over cat2
 */
template<class, class>
struct cat_times;
struct cat_times_friend {
  friend constexpr auto operator*(auto left, auto right)
      TUP20_ARROW(cat_times{left.make, cat2(left.tup, right.tup, left.make)})
};
template<class Make, class Tup>
struct cat_times : cat_times_friend {
  [[no_unique_address]] Make make;
  Tup                        tup;
  constexpr cat_times(Make mk, Tup tp) : make{mk}, tup{tp} {}
};
} // namespace

consteval auto cat_for(auto make_tup)
    TUP20_RET([make_tup](auto&&... tups) TUP20_NOEX(
        (cat_times{make_tup, TUP20_FWD(tups)} * ...).tup))
// ARROW crashes irony

[[maybe_unused]] constexpr auto cat = cat_for(make_tuple);
// SAY(cat(tuple{1, 2}, tuple{'a', 'b'}, tuple{3, 4}));

template<auto... I>
static constexpr auto
    apply_(std::index_sequence<I...>, auto&& f, auto&& tup)
        TUP20_ARROW(std::invoke(TUP20_FWD(f), get_n<I>(TUP20_FWD(tup))...))
constexpr auto apply(auto&& f, auto&& tup)
    TUP20_ARROW(apply_(std::make_index_sequence<size<decltype(tup)>>{},
                       TUP20_FWD(f),
                       TUP20_FWD(tup)))

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
#endif // TUPLE_INCLUDE_GUARD_HPP
