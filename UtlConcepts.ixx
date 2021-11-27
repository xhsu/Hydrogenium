module;

#include <array>
#include <concepts>

export module UtlConcepts;


#pragma region String-related

export template<typename T>
concept StringType = std::is_same_v<T, char> || std::is_same_v<T, char8_t> || std::is_same_v<T, wchar_t> || std::is_same_v<T, char16_t> || std::is_same_v<T, char32_t>;

#pragma endregion String-related

#pragma region Arithmetic-related

export template<typename T> concept Arithmetic = std::is_arithmetic_v<T>;

#pragma endregion Arithmetic-related

#pragma region Data structure related

export template<typename T> concept ProperIter = requires(T iter) { *iter++; };

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

export template<typename T> concept Array = _impl_ArrayDect<T>::value;

#pragma endregion Data structure related
