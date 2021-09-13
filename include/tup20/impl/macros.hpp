#ifndef MACROS_HPP_INCLUDE_GUARD
#define MACROS_HPP_INCLUDE_GUARD

#include "hedley.h"

#define TUP20_FWD(...) (static_cast<decltype(__VA_ARGS__)&&>(__VA_ARGS__))
#define TUP20_RET(...)                                                    \
  { return __VA_ARGS__; }

#define TUP20_NOEX(...)                                                   \
  noexcept(noexcept(__VA_ARGS__)) TUP20_RET(__VA_ARGS__)

#define TUP20_ARROW(...)                                                  \
  noexcept(noexcept(__VA_ARGS__))->decltype(__VA_ARGS__) TUP20_RET(__VA_ARGS__)

namespace tup20 { namespace fn_impl {
template<class T>
auto depend_id(auto x) TUP20_RET(x)

    // A dummy type that cannot be used.
    // This is for catching arity mismatches with FN expressions
    struct fake {
  template<class T>
  operator T() {
    static_assert(depend_id<T>(false), "Arity mismatch for FN expression.");
  }
};
}} // namespace tup20::fn_impl

#define TUP20_GENSYM(name) HEDLEY_CONCAT3(tup20_gensym, __COUNTER__, name)

#define TUP20_FN_(name, ...)                                              \
  <class HEDLEY_CONCAT(name, 0) = ::tup20::fn_impl::fake,                 \
   class HEDLEY_CONCAT(name, 1) = ::tup20::fn_impl::fake,                 \
   class HEDLEY_CONCAT(name, 2) = ::tup20::fn_impl::fake,                 \
   class HEDLEY_CONCAT(name, 3) = ::tup20::fn_impl::fake,                 \
   class HEDLEY_CONCAT(name, 4) = ::tup20::fn_impl::fake>(                \
      [[maybe_unused]] HEDLEY_CONCAT(name, 0) _0 = {},                    \
      [[maybe_unused]] HEDLEY_CONCAT(name, 1) _1 = {},                    \
      [[maybe_unused]] HEDLEY_CONCAT(name, 2) _2 = {},                    \
      [[maybe_unused]] HEDLEY_CONCAT(name, 3) _3 = {},                    \
      [[maybe_unused]] HEDLEY_CONCAT(name, 4) _4 = {}) {                  \
    [[maybe_unused]] auto& _ = _0;                                        \
    return __VA_ARGS__;                                                   \
  }
// ARROW([&](auto& _) ARROW(__VA_ARGS__)(_0)) as the body crashed clang-tidy
// TODO: how to make this noexcept-correct? These will probably be inlined
// so it's low priority

#define TUP20_FN(...) TUP20_FN_(GENSYM(fn_type), __VA_ARGS__)

#endif // MACROS_HPP_INCLUDE_GUARD
