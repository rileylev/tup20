include(${CMAKE_BINARY_DIR}/conan_paths.cmake)

# find_program(CLANGTIDY clang-tidy)
# if(CLANGTIDY)
#   set(CMAKE_CXX_CLANG_TIDY ${CLANGTIDY} -extra-arg=-Wno-unknown-warning-option)
# endif()

set(SANITIZERS "address")

if (CMAKE_GENERATOR MATCHES "Visual Studio")
else()
add_compile_options(
  -Werror
  -Wall
  -Wextra
  -Wshadow
  -Wnon-virtual-dtor
  -Wold-style-cast
  -Wunused
  -Wconversion
  -Wsign-conversion
  -Wnull-dereference
  -Wdouble-promotion
  )
endif()

list(JOIN SANITIZERS "," COMMA_SANITIZERS)

message("Using sanitizers: ${COMMA_SANITIZERS}")

if(SANITIZERS)
  add_compile_options(-fsanitize=${COMMA_SANITIZERS})
  add_link_options(-fsanitize=${COMMA_SANITIZERS})
endif()
