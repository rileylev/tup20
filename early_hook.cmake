# This defines the development build process (for me)
# This will set up CMAKE to work with conan without leaking the
# conan-related details into the core project definition.
#
# This file also sets up testing and options relevant for development
cmake_minimum_required(VERSION 3.17)

set(CMAKE_WARN_DEPRICATED ON)
set(CMAKE_FIND_PACKAGE_PREFER_CONFIG TRUE)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

enable_testing()

find_program(CCACHE ccache)
if(CCACHE)
  message(STATUS "Found ccache")
  set(CMAKE_CXX_COMPILER_LAUNCHER ${CCACHE})
else()
  message(STATUS "Couldn't find ccache")
endif()
