cmake_minimum_required(VERSION 3.20)

project(
  size_sorted_tuple
  DESCRIPTION "A tuple wrapper that sorts types to ensure tighter memory layout"
  VERSION 0.0.0
  LANGUAGES CXX)

  include(early_hook.cmake)

add_library(size_sorted_tuple INTERFACE)
add_library(size_sorted_tuple::size_sorted_tuple ALIAS size_sorted_tuple)
target_include_directories(size_sorted_tuple INTERFACE include)
target_compile_features(size_sorted_tuple INTERFACE cxx_std_20)

add_subdirectory(test)
