#ifndef MACROS_HPP_INCLUDE_GUARD
#define MACROS_HPP_INCLUDE_GUARD

#include "hedley.h"

/**
 * A macro that forwards its argument (as in std::forward)
 * std::forward requires we pass the argument's type as a template
 * parameter, (so it can know the value category) even though the compiler
 * knows the type at the site of the std::forward call. Repeating this is
 * verbose and error-prone, so automate it.
 */
#define TUP20_FWD(...) (static_cast<decltype(__VA_ARGS__)&&>(__VA_ARGS__))
// equivalent to std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

/*************************
 * Arrow function macros *
 *************************/
// Some languages let you express a function whose body is a single
// expression using an arrow, something like
//
// f(x) => x * x
//
// In c++, when the body of a function is a single simple expression, we
// can easily deduce the return type and noexcept-ness. The function
//
// f(x) => x * x
//
// could become
//
// auto f(x) noexcept(noexcept(x*x)) -> decltype(x*x){
//   return x*x;
// }
//
// This will have the correct return type and noexcept-ness, but requires
// duplicating the function's body in three places. Afaik, the easiest way
// to automate this duplication is with a macro.
//
// These are helpful for writing value wrappers, like tuple or optional.
// Because value wrappers have little of their own behavior, their
// functions' bodies often are a single expression. Furthermore,
// value wrappers should propagate noexcept-ness.
#define TUP20_RET(...)                                                    \
  { return __VA_ARGS__; }

#define TUP20_NOEX(...)                                                   \
  noexcept(noexcept(__VA_ARGS__)) TUP20_RET(__VA_ARGS__)

#define TUP20_ARROW(...)                                                  \
  noexcept(noexcept(__VA_ARGS__))->decltype(__VA_ARGS__) TUP20_RET(__VA_ARGS__)

/************************
 * FN for short lambdas *
 ************************/
// []FN(_+1)
// ==exapnds to==>
// [](auto _) { return _ + 1; }
//
// []FN(_*_)
// ==expands to==>
// [](auto _) { return _ * _; }
//
// Concision matters. It can make the difference between clarity and
// obscurity. A lightweight syntax for short lambda expressions facilitate
// functional programming. It is common to need a lambda that does very
// little: reorder some arguments, fix one input to a function, or call
// some operators. Needing to write the long-form lambda expression in
// these cases makes the code harder to write and, more importantly,
// read, especially when you need many of these close together.
//
// The following code implements this syntax, similar to boost's C++03
// lambdas, using a macro. Using a macro both makes the implementation
// simpler (no placeholder types with overloaded operators), and fits
// better within the language (lambda captures work the same)
namespace tup20 { namespace fn_impl {
template<class T>
/**
 * Artificially make a value depend on a template type parameter.
 * This prevents the compiler from eagerly evaluating a
 * static_assert(false, ...) in template code.
 */
auto depend_id(auto x) TUP20_RET(x)

/**
 * A dummy type that cannot be used.
 * This is for catching arity mismatches with FN expressions
 */
struct fake {
  template<class T>
  operator T() {
    static_assert(depend_id<T>(false),
                  "A FN expression was called without enough arguments");
  }
};
}} // namespace tup20::fn_impl

/**
 * Generate a unique identifier containing name. Useful to prevent names in
 * macro expansions from colliding with other names in the scope.
 *
 * see: Common lisp's gensym (http://clhs.lisp.se/Body/f_gensym.htm),
 */
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
// TODO: should we take arguments by ref? constref?

#define TUP20_FN(...) TUP20_FN_(TUP20_GENSYM(fn_type), __VA_ARGS__)

// []TUP20_FN(_3)(0) ==> compile error!!

#endif // MACROS_HPP_INCLUDE_GUARD
