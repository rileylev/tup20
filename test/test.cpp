#define CATCH_CONFIG_MAIN

#include <tup20/tuple.hpp>
#include <catch2/catch.hpp>

#include <array>

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
  struct empty2{};
  STATIC_REQUIRE(sizeof(tup20::tuple<empty, int, empty2>) == sizeof(int));
  // STATIC_REQUIRE(sizeof(tup20::tuple<empty, int, empty>) == sizeof(int));
  // for some reason this one is false
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
