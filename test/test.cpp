#define CATCH_CONFIG_MAIN

#include <tup20/tuple.hpp>
#include <catch2/catch.hpp>

#include <array>
#include <cassert>

// This is to test we get helpful compile errors. I will automate these
// eventually int bad_get(){
//   tup20::tuple<int,int> x;
//   return tup20::get_t<int>(x);
// }

TEST_CASE("tup20::tuple is trivially copyable if its components are") {
  STATIC_REQUIRE(
      std::is_trivially_copyable_v<tup20::tuple<int, int, float>>);
  STATIC_REQUIRE(
      std::is_trivially_copyable_v<tup20::tuple<int, std::array<float, 5>>>);
}

TEST_CASE("tup20::tuple's <=> uses lexographic comparison") {
  STATIC_REQUIRE((tup20::tuple{0, 0, 0} <=> tup20::tuple{0, 1, -1}) < 0);
  STATIC_REQUIRE((tup20::tuple{0, 0, 0} <=> tup20::tuple{0, 0, 0}) == 0);
}

TEST_CASE("(distinct) empty types are compressed") {
  struct empty {};
  struct empty2 {};
  STATIC_REQUIRE(sizeof(tup20::tuple<empty, int, empty2>) == sizeof(int));
  // STATIC_REQUIRE(sizeof(tup20::tuple<empty, int, empty>) ==
  // sizeof(int)); for some reason this one is false
}

TEST_CASE("assigning to a tuple assigns componentwise") {
  constexpr auto tup = [] {
    tup20::tuple<int, double> t;
    t = {3, 2.0};
    return t;
  }();
  STATIC_REQUIRE(tup20::get_n<0>(tup) == 3);
  STATIC_REQUIRE(tup20::get_n<1>(tup) == 2.0);
}

TEST_CASE("std::tuple_size_v can get the size of a tup20::tuple") {
  STATIC_REQUIRE(std::tuple_size_v<tup20::tuple<int, int, int>> == 3);
}

#include <tup20/permute.hpp>
template<class To, class From>
constexpr To narrow(From from) {
  To to = static_cast<To>(from);
  assert(std::cmp_equal(to, from));
  return to;
}

template<std::convertible_to<std::size_t>... Args>
constexpr auto make_permutation(Args... args)
    TUP20_ARROW(tup20::permutation<sizeof...(Args)>{
        narrow<std::size_t>(args)...})

TEST_CASE("invert calculates the inverse of a permutation") {
  constexpr auto circ_right = make_permutation(1, 2, 3, 0);
  constexpr auto circ_left  = make_permutation(3, 0, 1, 2);
  // identity
  STATIC_REQUIRE(tup20::inverse(make_permutation(0, 1, 2))
                 == make_permutation(0, 1, 2));
  // circle right -> circle left
  STATIC_REQUIRE(tup20::inverse(circ_right) == circ_left);
}

TEST_CASE("permute_tuple calculates what you get from permuting a tuple's "
          "elements") {
  constexpr auto circ_right = make_permutation(1, 2, 3, 0);
  STATIC_REQUIRE(tup20::permute_tuple<circ_right>(tup20::tuple{0, 1, 2, 3})
                 == tup20::tuple{3, 0, 1, 2});
}

TEST_CASE("will tuple_cat crash clang?") {
  STATIC_REQUIRE(tup20::cat(tup20::tuple{'a', 'b'}, tup20::tuple{'c', 'd'})
                 == tup20::tuple{'a', 'b', 'c', 'd'});
}

#include <tup20/size_sorted_tuple.hpp>
TEST_CASE("size_descending_permutation calculates the permutation "
          "that will sort the types in order of descending sizes") {
  STATIC_REQUIRE(
      tup20::size_descending_permutation<int,
                                         char,
                                         void*> == make_permutation(1, 2, 0));
}

TEST_CASE("We can check for a bad_overload using decltype without "
          "triggering the static_assert.") {
  STATIC_REQUIRE(
      std::is_same_v<decltype(get<3>(tup20::tuple{})), tup20::bad_overload>);
  // but touching it breaks
  // constexpr auto x = get<3>(tup20::tuple{}); // -> static_assert fires
}
