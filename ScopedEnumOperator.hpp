/*
* Provide arithmetic operators for scoped enumerators.
* The goals are make scoped enumerators work like regular int.
* Oct 23 2021
*/

#pragma once

#include <compare>
#include <concepts>

template <typename E>	// Why lacking this?!
concept Enumerator = std::is_enum_v<E>;

template <typename E>
concept ScopedEnumerator = std::is_scoped_enum_v<E>;

template <typename T>	// Why lacking this 2: Electric boogaloo
concept _internal_SEO_Arithmetic = std::is_arithmetic_v<T>;

template <typename T>	// As an extent of std::integral
concept IntegerOrUnscopedEnum = std::integral<T> || (std::is_enum_v<T> && !std::is_scoped_enum_v<T>);

template <typename T>
concept IntegerOrEnum = std::integral<T> || std::is_enum_v<T>;

template <typename T>
concept ArithmeticOrEnum = std::is_arithmetic_v<T> || std::is_enum_v<T>;

template <typename T>
struct _DecayOrUnderlying_Helper : public std::false_type {};

template<_internal_SEO_Arithmetic T>
struct _DecayOrUnderlying_Helper<T>
{
	using type = std::decay_t<T>;
};

template<Enumerator T>
struct _DecayOrUnderlying_Helper<T>
{
	using type = std::underlying_type_t<T>;
};

template <typename T>
using DecayOrUnderlying = _DecayOrUnderlying_Helper<T>::type;



#pragma region _internal_SEO_Arithmetic operators

// Addition
template<ArithmeticOrEnum T, ArithmeticOrEnum U>
constexpr decltype(auto) operator+ (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) + DecayOrUnderlying<U>(rhs);
}

// Subtraction
template<ArithmeticOrEnum T, ArithmeticOrEnum U>
constexpr decltype(auto) operator- (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) - DecayOrUnderlying<U>(rhs);
}

// Unary plus (integer promotion)
// Reference: https://stackoverflow.com/questions/727516/what-does-the-unary-plus-operator-do
template<ScopedEnumerator E>
constexpr decltype(auto) operator+ (E rhs) noexcept
{
	using T = std::underlying_type_t<E>;

	if constexpr (sizeof(T) < sizeof(signed int))
	{
		return static_cast<signed int>(rhs);
	}
	else
	{
		return static_cast<T>(rhs);
	}
}

// Unary minus (additive inverse)
template<ScopedEnumerator E>
constexpr decltype(auto) operator- (E rhs) noexcept { return -static_cast<std::make_signed_t<E>>(rhs); }

// Multiplication
template<ArithmeticOrEnum T, ArithmeticOrEnum U>
constexpr decltype(auto) operator* (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) * DecayOrUnderlying<U>(rhs);
}

// Division
template<ArithmeticOrEnum T, ArithmeticOrEnum U>
constexpr decltype(auto) operator/ (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) / DecayOrUnderlying<U>(rhs);
}

// Modulo (integer remainder)
template<IntegerOrEnum T, IntegerOrEnum U>
constexpr decltype(auto) operator% (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) % DecayOrUnderlying<U>(rhs);
}

// Increment prefix
template<ScopedEnumerator E>
constexpr E& operator++ (E& i) noexcept { return i = (E)(static_cast<std::underlying_type_t<E>>(i) + 1); }

// Decrement prefix
template<ScopedEnumerator E>
constexpr E& operator-- (E& i) noexcept { return i = (E)(static_cast<std::underlying_type_t<E>>(i) - 1); }

#pragma endregion _internal_SEO_Arithmetic operators

#pragma region Comparison operators/relational operators

// Three-way comparison
// The comparison between floating-point numbers are std::partial_ordering whereas integers use std::strong_ordering.
template<ArithmeticOrEnum T, ArithmeticOrEnum U>
constexpr decltype(auto) operator<=> (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) <=> DecayOrUnderlying<U>(rhs);
}

// Equal to (with C++20 it will generate a operator!= for you.)
template<ArithmeticOrEnum T, ArithmeticOrEnum U>
constexpr bool operator== (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) == DecayOrUnderlying<U>(rhs);
}

#pragma endregion Comparison operators/relational operators

#pragma region Logical operators

// Logical negation (NOT)
template<ScopedEnumerator E>
constexpr bool operator! (const E& lhs) noexcept { return std::underlying_type_t<E>(lhs) == 0; }

// Logical AND
template <typename T, typename U>
constexpr bool operator&& (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	if constexpr (std::is_scoped_enum_v<T> && !std::is_scoped_enum_v<U>)
		return std::underlying_type_t<T>(lhs) && rhs;
	else if constexpr (!std::is_scoped_enum_v<T> && std::is_scoped_enum_v<U>)
		return lhs && std::underlying_type_t<U>(rhs);
	else if constexpr (std::is_scoped_enum_v<T> && std::is_scoped_enum_v<U>)
		return std::underlying_type_t<T>(lhs) && std::underlying_type_t<U>(rhs);
}

// Logical OR
template <typename T, typename U>
constexpr bool operator|| (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	if constexpr (std::is_scoped_enum_v<T> && !std::is_scoped_enum_v<U>)
		return std::underlying_type_t<T>(lhs) || rhs;
	else if constexpr (!std::is_scoped_enum_v<T> && std::is_scoped_enum_v<U>)
		return lhs || std::underlying_type_t<U>(rhs);
	else if constexpr (std::is_scoped_enum_v<T> && std::is_scoped_enum_v<U>)
		return std::underlying_type_t<T>(lhs) || std::underlying_type_t<U>(rhs);
}

#pragma endregion Logical operators

#pragma region Bitwise operators

// Bitwise NOT
template<ScopedEnumerator E>
constexpr E operator~ (const E& rhs) noexcept { return (E)~static_cast<std::underlying_type_t<E>>(rhs); }

// Bitwise OR
template<ScopedEnumerator E>
constexpr E operator| (const E& lhs, const E& rhs) noexcept { return (E)(std::underlying_type_t<E>(lhs) | std::underlying_type_t<E>(rhs)); }

// Bitwise AND
template<ScopedEnumerator E>
constexpr E operator& (const E& lhs, const E& rhs) noexcept { return (E)(std::underlying_type_t<E>(lhs) & std::underlying_type_t<E>(rhs)); }

// Bitwise XOR
template<ScopedEnumerator E>
constexpr E operator^ (const E& lhs, const E& rhs) noexcept { return (E)(std::underlying_type_t<E>(lhs) ^ std::underlying_type_t<E>(rhs)); }

// Bitwise left shift
template<IntegerOrEnum T, IntegerOrEnum U>
constexpr decltype(auto) operator<< (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) << DecayOrUnderlying<U>(rhs);
}

// Bitwise right shift
template<IntegerOrEnum T, IntegerOrEnum U>
constexpr decltype(auto) operator>> (const T& lhs, const U& rhs) noexcept requires(std::is_scoped_enum_v<T> || std::is_scoped_enum_v<U>)
{
	return DecayOrUnderlying<T>(lhs) >> DecayOrUnderlying<U>(rhs);
}

#pragma endregion Bitwise operators

#pragma region Assignment operators

template<ScopedEnumerator E>
constexpr E& operator+= (E& lhs, const auto& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) + std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator-= (E& lhs, const auto& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) - std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator*= (E& lhs, const auto& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) * std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator/= (E& lhs, const auto& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) / std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator%= (E& lhs, const IntegerOrEnum auto& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) % std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator|= (E& lhs, const E& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) | std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator&= (E& lhs, const E& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) & std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator^= (E& lhs, const E& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) ^ std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator<<= (E& lhs, const IntegerOrEnum auto& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) << std::underlying_type_t<E>(rhs)); }

template<ScopedEnumerator E>
constexpr E& operator>>= (E& lhs, const IntegerOrEnum auto& rhs) noexcept { return lhs = (E)(std::underlying_type_t<E>(lhs) >> std::underlying_type_t<E>(rhs)); }

#pragma endregion Assignment operators
