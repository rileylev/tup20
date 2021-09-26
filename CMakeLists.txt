cmake_minimum_required(VERSION 3.20)

project(
  tup20
  DESCRIPTION "A tuple wrapper that sorts types to ensure tighter memory layout"
  VERSION 0.0.0
  LANGUAGES CXX)

  include(early_hook.cmake)

add_library(tup20 INTERFACE)
add_library(tup20::tup20 ALIAS tup20)
target_include_directories(tup20 INTERFACE include)
target_compile_features(tup20 INTERFACE cxx_std_20)

add_subdirectory(test)
