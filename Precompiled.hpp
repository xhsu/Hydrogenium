
#pragma warning(push)
#pragma warning(1 : 4668) // 'MEOW' is not defined as a preprocessor macro, replacing with '0' for '#if/#elif'

// All STL headers should protect themselves from macroized new.
#if !(defined(__CUDACC__) && defined(__clang__))
#pragma push_macro("new")
#undef new
#define new WILL NOT COMPILE
#endif // !(defined(__CUDACC__) && defined(__clang__))

#ifndef _SILENCE_CXX17_C_HEADER_DEPRECATION_WARNING
#define _SILENCE_CXX17_C_HEADER_DEPRECATION_WARNING
#endif // !defined(_SILENCE_CXX17_C_HEADER_DEPRECATION_WARNING)

#define _SILENCE_CXX20_CISO646_REMOVED_WARNING
#define _SILENCE_EXPERIMENTAL_FILESYSTEM_DEPRECATION_WARNING
#define _SILENCE_STDEXT_HASH_DEPRECATION_WARNINGS

// Core STL Headers
#include <bit>
#include <compare>
#include <concepts>
#include <coroutine>
#include <initializer_list>
#include <limits>
#include <numbers>
#include <ratio>
#include <source_location>
#include <stdfloat>
#include <tuple>
#include <type_traits>
#include <utility>
#include <version>

// Core C Wrapper Headers
#include <cassert>
#include <cctype>
#include <cerrno>
#include <cfenv>
#include <cfloat>
#include <cinttypes>
#include <climits>
#include <clocale>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cuchar>
#include <cwchar>
#include <cwctype>

// Non-Core STL Headers
#include <algorithm>
#include <any>
#include <array>
#include <bitset>
#include <charconv>
#include <chrono>
#include <codecvt>
#include <complex>
#include <deque>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <forward_list>
#include <fstream>
#include <functional>
#include <generator>
#include <hash_map>
#include <hash_set>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <iterator>
#include <list>
#include <locale>
#include <map>
#include <mdspan>
#include <memory>
#include <memory_resource>
#include <new>
#include <numeric>
#include <optional>
#include <ostream>
#include <print>
#include <queue>
#include <random>
#include <ranges>
#include <regex>
#include <scoped_allocator>
#include <set>
#include <span>
#include <spanstream>
#include <sstream>
#include <stack>
#include <stacktrace>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <string_view>
#include <strstream>
#include <syncstream>
#include <system_error>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <unordered_set>
#include <valarray>
#include <variant>
#include <vector>

#ifndef _M_CEE_PURE
#include <__msvc_cxx_stdatomic.hpp>
#include <atomic>
#include <barrier>
#include <condition_variable>
#include <execution>
#include <future>
#include <latch>
#include <mutex>
#include <semaphore>
#include <shared_mutex>
#include <stop_token>
#include <thread>
#endif // !defined(_M_CEE_PURE)

#if !(defined(__CUDACC__) && defined(__clang__))
#pragma pop_macro("new")
#endif // !(defined(__CUDACC__) && defined(__clang__))

#pragma warning(pop)

// {fmt} lib
#include <fmt/color.h>
#include <fmt/compile.h>
#include <fmt/ranges.h>
#include <fmt/std.h>
#include <fmt/xchar.h>
