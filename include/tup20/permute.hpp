#ifndef TUP20_PERMUTE_HPP_INCLUDE_GUARD
#define TUP20_PERMUTE_HPP_INCLUDE_GUARD

#include "impl/macros.hpp"
#include "tuple.hpp"

#include <array>

namespace tup20 {
/**
 * A dense representation of Sn (permutation group of {0,1,... n-1})
 * σ: i ↦ σ[i]
 */
template<std::size_t N>
using permutation = std::array<std::size_t, N>;

/**
 * Inverts a permutation
 */
template<std::size_t N>
constexpr permutation<N> invert(permutation<N> const σ) noexcept {
  //  σ⁻¹σ   = id
  //  σ⁻¹σ i = i
  permutation<N> inv;
  for(std::size_t i = 0; i < N; ++i) inv[σ[i]] = i;
  return inv;
}

/*****************
 * σx[i]=x[σ⁻¹i] *
 *****************/
// σ ∈ Sn (permutation group for n elements)
// (σx)[σi] = x[i]
// (σx)[i] = x[σ⁻¹i]
// Best shown with an example:
// x = (a,b,c)
// σ = (0 → 1, 1 → 2, 2 → 0)  then σ⁻¹ = (0 → 2, 1 → 0, 2 → 1)
// σx = (c,a,b) = (x[2], x[0], x[1]) = (x[σ⁻¹ 0], x[σ⁻¹ 1], x[σ⁻¹ 2])
template<auto σ, auto... Is>
static constexpr auto
    unpermute_tuple_(auto const& tup, std::index_sequence<Is...>)
        TUP20_ARROW(tuple{tup20::get_n<σ[Is]>(tup)...})
template<auto σ>
/**
 * Unpermutes tup by σ, that is, permutes the elements of tup by σ⁻¹.
 */
constexpr auto unpermute_tuple(auto const& tup)
    TUP20_ARROW(unpermute_tuple_<σ>(
        tup,
        std::make_index_sequence<
            tup20::size<std::remove_cvref_t<decltype(tup)>>>{}));
// alternate approach would be to permute the index sequence by σ before
// passing it along
/**
 * Permute the elements of tup by σ
 */
template<auto σ>
constexpr auto permute_tuple(auto const& tup)
    TUP20_ARROW(unpermute_tuple<invert(σ)>(tup))

namespace impl {
template<auto σ, class... Ts>
using permute_tuple_t =
    decltype(permute_tuple<σ>(std::declval<tuple<Ts...>>()));

struct permuted_tuple_frieds {
  /* x[i]=σx[σi]; external[i]=underlying_[σi] */
  template<auto n>
  friend constexpr auto get(auto&& tup)
      TUP20_ARROW(get_n<std::remove_cvref_t<decltype(tup)>::σ[n]>(
          TUP20_FWD(tup.underlying_)))

  template<class T>
  friend constexpr auto get(auto&& tup)
      TUP20_ARROW(get_t<T>(TUP20_FWD(tup.underlying_)))
};
} // namespace impl

// present an interface as if storage hasn't been permuted
// x= eXternal
// σ x = internal (underlying)
// x[i] = σx[σi]
/**
 * A tuple containing elements Ts... where the storage is *transparently
 * permuted* by σ: elements are stored in permuted order (by σ) but are
 * accessed in declaration order (the order they show up in Ts...)
 *
 * This is for storage optimization. For example, structure packing by
 * sorting members by descending size.
 */
template<auto σ_, class... Ts>
class storage_permuted_tuple : impl::permuted_tuple_frieds {
  using typelist = tuple<Ts...>;

 public:
  static constexpr auto size = sizeof...(Ts);

  template<auto N>
  using nth_t = typename typelist::template nth_t<N>;

 private:
  static constexpr auto σ = σ_;
  using underlying_t      = impl::permute_tuple_t<σ, Ts...>;
  underlying_t underlying_;

 public:
  // TODO: right semantics for convert vs singleton?
  // convert seems more likely to be what a user meant
  // TODO: explicit?
  constexpr storage_permuted_tuple(tuple<Ts...> const& args) noexcept(
      noexcept(underlying_t{permute_tuple<σ>(std::move(args))}))
      : underlying_{permute_tuple<σ>(std::move(args))} {}

  template<class... Args>
  constexpr TUP20_IMPLICIT((std::is_convertible_v<Args&&, Ts> and ...))
      storage_permuted_tuple(Args&&... args) noexcept(
          noexcept(storage_permuted_tuple{tuple{TUP20_FWD(args)...}}))
      : storage_permuted_tuple{tuple{TUP20_FWD(args)...}} {}
};
} // namespace tup20
#endif // TUP20_PERMUTE_HPP_INCLUDE_GUARD
