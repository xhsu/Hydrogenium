module;

#include <array>
#include <concepts>
#include <fstream>
#include <iostream>
#include <string>

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

export template<typename T>
concept ResizableContainer = requires(T t)
{
	{ t.size() } -> std::integral;
	{ t.emplace_back };
};

template<typename T>
struct _impl_ArrayDect : public std::false_type {};

template<typename T, size_t N>
struct _impl_ArrayDect<std::array<T, N>> : public std::true_type {};

template<typename T, size_t N>
struct _impl_ArrayDect<T[N]> : public std::true_type {};

export template<typename T>
concept Array = _impl_ArrayDect<T>::value;

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

#pragma endregion Type traits
