module;

#include <array>
#include <concepts>
#include <deque>
#include <forward_list>
#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

export module UtlConcepts;


#pragma region Simple basic types

export using uint8 = unsigned __int8;
export using uint16 = unsigned __int16;
export using uint32 = unsigned __int32;
export using uint64 = unsigned __int64;

#pragma endregion Simple basic types

#pragma region String-related

export template<typename T>
concept CharacterType = std::is_same_v<T, char> || std::is_same_v<T, char8_t> || std::is_same_v<T, wchar_t> || std::is_same_v<T, char16_t> || std::is_same_v<T, char32_t>;

#pragma endregion String-related

#pragma region Arithmetic-related

export template<typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

#pragma endregion Arithmetic-related

#pragma region Data structure related

export template<typename T>
concept ProperIter = requires(T iter) { *iter++; };

export template<typename T>
concept Iterable = ProperIter<decltype(std::begin(std::declval<T>()))> && ProperIter<decltype(std::end(std::declval<T>()))>;
/* This cannot be used in identifying built-in array. Hence it was removed.
requires(T t)
{
	{ std::begin(t) } -> ProperIter;
	{ std::end(t) } -> ProperIter;
};*/

template <typename T>
constexpr bool _impl_ResizableContainer = false;

template <typename T>
constexpr bool _impl_ResizableContainer<std::vector<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::deque<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::forward_list<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::list<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::set<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::multiset<T>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::map<T, U>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::multimap<T, U>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::unordered_set<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::unordered_multiset<T>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::unordered_map<T, U>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::unordered_multimap<T, U>> = true;

export template <typename T>
concept ResizableContainer = _impl_ResizableContainer<T>;

template<typename T>
struct _impl_ArrayDect : public std::false_type {};

template<typename T, size_t N>
struct _impl_ArrayDect<std::array<T, N>> : public std::true_type {};

template<typename T, size_t N>
struct _impl_ArrayDect<T[N]> : public std::true_type {};

export template<typename T>
concept Array = _impl_ArrayDect<T>::value;

export template<typename T>
concept Pointer = std::is_pointer_v<T>;

#pragma endregion Data structure related

#pragma region Template detector

template<typename T>
struct _impl_OutStreamDect : public std::false_type {};

template<typename T>
struct _impl_OutStreamDect<std::basic_ostream<T, std::char_traits<T>>> : public std::true_type {};

template<typename T>
struct _impl_OutStreamDect<std::basic_ofstream<T, std::char_traits<T>>> : public std::true_type {};

export template<typename T>
concept OStream = _impl_OutStreamDect<T>::value;

template<typename T>
struct _impl_InStreamDect : public std::false_type {};

template<typename T>
struct _impl_InStreamDect<std::basic_istream<T, std::char_traits<T>>> : public std::true_type {};

template<typename T>
struct _impl_InStreamDect<std::basic_ifstream<T, std::char_traits<T>>> : public std::true_type {};

export template<typename T>
concept IStream = _impl_InStreamDect<T>::value;

template<typename T>
struct _impl_StringDect : public std::false_type {};

template<typename T>
struct _impl_StringDect<std::basic_string<T, std::char_traits<T>, std::allocator<T>>> : public std::true_type {};

export template<typename T>
concept StlString = _impl_StringDect<T>::value;

template<typename T>
struct _impl_StringViewDect : public std::false_type {};

template<typename T>
struct _impl_StringViewDect<std::basic_string_view<T, std::char_traits<T>>> : public std::true_type {};

export template<typename T>
concept StlStringView = _impl_StringViewDect<T>::value;

export template<typename T>
concept StlStringClass = _impl_StringDect<T>::value || _impl_StringViewDect<T>::value;

#pragma endregion Template detector

#pragma region Type traits

template<typename T>
struct _impl_NonVoidType : public std::true_type {};

template<>
struct _impl_NonVoidType<void> : public std::false_type {};

export template<typename T>
concept NonVoid = _impl_NonVoidType<T>::value;

export template<typename T>
concept HasIndexOperator = requires(T t) { {t[std::declval<size_t>()]} -> NonVoid; };

template <typename T, typename... Tys>
constexpr bool _impl_AnySame = (std::is_same_v<T, Tys> || ...) || _impl_AnySame<Tys...>;

template <typename T>
constexpr bool _impl_AnySame<T> = false;

export template <typename T, typename... Tys>
concept AnySame = _impl_AnySame<T, Tys...>;

#pragma endregion Type traits
