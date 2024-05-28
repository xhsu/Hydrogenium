/*
* 
* Provide arithmetic operators for scoped enumerators.
* The goals are make scoped enumerators work like regular int.
* 
* Created at: Oct 23 2021
* 
*/

#define HYDROGENIUM_SEO 20240528L

#pragma once

#include <compare>
#include <concepts>
#include <type_traits>
#include <utility>

template <typename E>	// Why lacking this?!
concept Enumerator = std::is_enum_v<E>;

template <typename E>
concept ScopedEnumerator = std::is_scoped_enum_v<E>;

template <typename T>	// As an extent of std::integral
concept IntegerOrUnscopedEnum = std::integral<T> || (std::is_enum_v<T> && !std::is_scoped_enum_v<T>);

template <typename T>
concept IntegerOrEnum = std::integral<T> || std::is_enum_v<T>;

template <typename T>
concept ArithmeticOrEnum = std::is_arithmetic_v<T> || std::is_enum_v<T>;

template <typename T, typename U>
struct EnumOpRetType2;

template <Enumerator T>
struct EnumOpRetType2<T, T> final { using type = T; };

template <Enumerator T, Enumerator U> requires (!std::is_same_v<T, U>)
struct EnumOpRetType2<T, U> final { using type = std::common_type_t<std::underlying_type_t<T>, std::underlying_type_t<U>>; };

template <ArithmeticOrEnum T, ArithmeticOrEnum U> requires (std::is_enum_v<T> && !std::is_enum_v<U>)
struct EnumOpRetType2<T, U> final { using type = T; };

template <ArithmeticOrEnum T, ArithmeticOrEnum U> requires (!std::is_enum_v<T>&& std::is_enum_v<U>)
struct EnumOpRetType2<T, U> final { using type = U; };

template <ArithmeticOrEnum T, ArithmeticOrEnum U>
using EnumOpRetType = EnumOpRetType2<T, U>::type;

__forceinline constexpr auto UnderlyingOrIdentity(ArithmeticOrEnum auto i) noexcept
{
	if constexpr (requires{ { std::to_underlying(i) } -> std::integral; })
	{
		return std::to_underlying(i);
	}
	else
	{
		return i;
	}
}

namespace Hydrogenium::UnitTest
{
	enum struct myenum : int16_t
	{
		e0,
		e1,
		e2,
		e3,
		e4,
		e5,
		e6,
	};

	enum struct myen2 : uint8_t
	{
		e0 = 0,
		e1 = (1 << 0),
		e2 = (1 << 1),
		e4 = (1 << 2),
		e8 = (1 << 3),
		e16 = (1 << 4),
		e32 = (1 << 5),
	};

	static_assert(std::is_same_v<EnumOpRetType<myenum, myenum>, myenum>);
	static_assert(std::is_same_v<EnumOpRetType<myenum, myen2>, int>);
	static_assert(std::is_same_v<EnumOpRetType<myenum, double>, myenum>);
	static_assert(std::is_same_v<EnumOpRetType<int, myen2>, myen2>);

	template <typename T, typename U>
	concept GoodForCalc = requires { typename EnumOpRetType2<T, U>::type; };	// requires expression will yield an error if it's not in a template.
	static_assert(!GoodForCalc<int, int>);
}



#pragma region Arithmetic operators

// Addition
template <ArithmeticOrEnum T, ArithmeticOrEnum U>
[[nodiscard]]
constexpr auto operator+ (T lhs, U rhs) noexcept -> EnumOpRetType<T, U>
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	return
		static_cast<EnumOpRetType<T, U>>(
			UnderlyingOrIdentity(lhs) + UnderlyingOrIdentity(rhs)
		);
}

// Subtraction
template <ArithmeticOrEnum T, ArithmeticOrEnum U>
[[nodiscard]]
constexpr auto operator- (T lhs, U rhs) noexcept -> EnumOpRetType<T, U>
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	return
		static_cast<EnumOpRetType<T, U>>(
			UnderlyingOrIdentity(lhs) - UnderlyingOrIdentity(rhs)
		);
}

// Unary plus (integer promotion)
// Reference: https://stackoverflow.com/questions/727516/what-does-the-unary-plus-operator-do
[[nodiscard]]
constexpr decltype(auto) operator+ (ScopedEnumerator auto rhs) noexcept
{
	if constexpr (sizeof(decltype(rhs)) < sizeof(signed int))
	{
		return static_cast<signed int>(rhs);
	}
	else
	{
		return std::to_underlying(rhs);
	}
}

// Unary minus (additive inverse)
template <ScopedEnumerator E>
[[nodiscard]]
constexpr auto operator- (E rhs) noexcept -> std::make_signed_t<std::underlying_type_t<E>>
{
	return -static_cast<std::make_signed_t<std::underlying_type_t<E>>>(rhs);
}

// Multiplication
template <ArithmeticOrEnum T, ArithmeticOrEnum U>
[[nodiscard]]
constexpr auto operator* (T lhs, U rhs) noexcept -> EnumOpRetType<T, U>
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	return
		static_cast<EnumOpRetType<T, U>>(
			UnderlyingOrIdentity(lhs) * UnderlyingOrIdentity(rhs)
		);
}

// Division
template <ArithmeticOrEnum T, ArithmeticOrEnum U>
[[nodiscard]]
constexpr auto operator/ (T lhs, U rhs) noexcept -> EnumOpRetType<T, U>
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	return
		static_cast<EnumOpRetType<T, U>>(
			UnderlyingOrIdentity(lhs) / UnderlyingOrIdentity(rhs)
		);
}

// Modulo (integer remainder)
template <IntegerOrEnum T, IntegerOrEnum U>
[[nodiscard]]
constexpr auto operator% (T lhs, U rhs) noexcept -> EnumOpRetType<T, U>
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	return
		static_cast<EnumOpRetType<T, U>>(
			UnderlyingOrIdentity(lhs) % UnderlyingOrIdentity(rhs)
		);
}

// Increment prefix
template <ScopedEnumerator E>
constexpr E& operator++ (E& i) noexcept
{
	return i = (E)(std::to_underlying(i) + 1);
}

// Decrement prefix
template <ScopedEnumerator E>
constexpr E& operator-- (E& i) noexcept
{
	return i = (E)(std::to_underlying(i) - 1);
}

namespace Hydrogenium::UnitTest
{
	static_assert(myen2::e1 + myenum::e1 == 2);
	static_assert(myen2::e1 - 1.0 == myen2::e0);

	static_assert(+myenum::e1 == 1);
	static_assert(-myenum::e1 == -1);

	static_assert(myenum::e2 * myen2::e2 == 4);
	static_assert(myenum::e5 / myen2::e2 == 2);
	static_assert(myenum::e5 % myen2::e2 == 1);

	consteval bool UniTest_SEO_Prefix()
	{
		myenum e = Hydrogenium::UnitTest::myenum::e0;
		++e;
		--e;

		return e == myenum::e0;
	}
	static_assert(UniTest_SEO_Prefix());
}

#pragma endregion Arithmetic operators

#pragma region Comparison operators/relational operators

// Three-way comparison
// The comparison between floating-point numbers are std::partial_ordering whereas integers use std::strong_ordering.
template <ArithmeticOrEnum T, ArithmeticOrEnum U>
[[nodiscard]]
constexpr decltype(auto) operator<=> (T lhs, U rhs) noexcept
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	return UnderlyingOrIdentity(lhs) <=> UnderlyingOrIdentity(rhs);
}

// Equal to (with C++20 it will generate a operator!= for you.)
template <ArithmeticOrEnum T, ArithmeticOrEnum U>
[[nodiscard]]
constexpr bool operator== (T lhs, U rhs) noexcept
	requires ((std::is_enum_v<T> || std::is_enum_v<U>) && !std::is_same_v<T, U>)
{
	return UnderlyingOrIdentity(lhs) == UnderlyingOrIdentity(rhs);
}

namespace Hydrogenium::UnitTest
{
	static_assert(myenum::e0 == 0 && myen2::e1 != 0);
	static_assert(myenum::e1 < 1.1f && myen2::e2 >= 2.0);
}

#pragma endregion Comparison operators/relational operators

#pragma region Logical operators

// Logical negation (NOT)

// Logical AND

// Logical OR

// Luna: 05/28/2024
// I really shouldn't do this.
// Noramlly it would be treated as int, so being able to test like a bool. But not here,
//	if operator|| or && gets overloaded, it will actually breaks all logic test in practice.

#pragma endregion Logical operators

#pragma region Bitwise operators

// Bitwise NOT
template <ScopedEnumerator E>
[[nodiscard]]
constexpr E operator~ (E rhs) noexcept
{
	return static_cast<E>(~std::to_underlying(rhs));
}

// Bitwise OR
template <ScopedEnumerator E>
[[nodiscard]]
constexpr E operator| (E lhs, E rhs) noexcept
{
	return static_cast<E>(std::to_underlying(lhs) | std::to_underlying(rhs));
}

// Bitwise AND
template <ScopedEnumerator E>
[[nodiscard]]
constexpr E operator& (E lhs, E rhs) noexcept
{
	return static_cast<E>(std::to_underlying(lhs) & std::to_underlying(rhs));
}

// Bitwise XOR
template <ScopedEnumerator E>
[[nodiscard]]
constexpr E operator^ (E lhs, E rhs) noexcept
{
	return static_cast<E>(std::to_underlying(lhs) ^ std::to_underlying(rhs));
}

// Bitwise left shift
template <IntegerOrEnum T, IntegerOrEnum U>
[[nodiscard]]
constexpr auto operator<< (T lhs, U rhs) noexcept -> T
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	auto const shift_val = UnderlyingOrIdentity(rhs);

	[[unlikely]]
	if (std::cmp_less(shift_val, 0) || std::cmp_greater(shift_val, sizeof(T) * 8))
		std::unreachable();

	return
		static_cast<T>(
			UnderlyingOrIdentity(lhs) << shift_val
		);
}

// Bitwise right shift
template <IntegerOrEnum T, IntegerOrEnum U>
[[nodiscard]]
constexpr auto operator>> (T lhs, U rhs) noexcept -> T
	requires (std::is_enum_v<T> || std::is_enum_v<U>)
{
	auto const shift_val = UnderlyingOrIdentity(rhs);

	[[unlikely]]
	if (std::cmp_less(shift_val, 0) || std::cmp_greater(shift_val, sizeof(T) * 8))
		std::unreachable();

	return
		static_cast<T>(
			UnderlyingOrIdentity(lhs) >> shift_val
		);
}

namespace Hydrogenium::UnitTest
{
	inline constexpr auto myen2_all_flags = myen2::e1 | myen2::e2 | myen2::e4 | myen2::e8 | myen2::e16 | myen2::e32;

	static_assert((bool)(myen2_all_flags & myen2::e1));
	static_assert(!(bool)(myen2::e1 & myen2::e2));
	static_assert(!(bool)(myen2_all_flags & ~myen2_all_flags));
	static_assert((myenum::e3 ^ myenum::e5) == myenum::e6);

	static_assert((myen2::e1 << 1) == myen2::e2);
	static_assert((myen2::e1 << myenum::e2) == myen2::e4);
	static_assert((4 >> myenum::e2) == 1);
}

#pragma endregion Bitwise operators

#pragma region Assignment operators

template <ScopedEnumerator E, ArithmeticOrEnum T>
constexpr E& operator+= (E& lhs, T rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) + UnderlyingOrIdentity(rhs)); }

template <ScopedEnumerator E, ArithmeticOrEnum T>
constexpr E& operator-= (E& lhs, T rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) - UnderlyingOrIdentity(rhs)); }

template <ScopedEnumerator E, ArithmeticOrEnum T>
constexpr E& operator*= (E& lhs, T rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) * UnderlyingOrIdentity(rhs)); }

template <ScopedEnumerator E, ArithmeticOrEnum T>
constexpr E& operator/= (E& lhs, T rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) / UnderlyingOrIdentity(rhs)); }

template <ScopedEnumerator E, IntegerOrEnum T>
constexpr E& operator%= (E& lhs, T rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) % UnderlyingOrIdentity(rhs)); }

template <ScopedEnumerator E>
constexpr E& operator|= (E& lhs, E rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) | std::to_underlying(rhs)); }

template <ScopedEnumerator E>
constexpr E& operator&= (E& lhs, E rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) & std::to_underlying(rhs)); }

template <ScopedEnumerator E>
constexpr E& operator^= (E& lhs, E rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) ^ std::to_underlying(rhs)); }

template <ScopedEnumerator E, IntegerOrEnum T>
constexpr E& operator<<= (E& lhs, T rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) << UnderlyingOrIdentity(rhs)); }

template <ScopedEnumerator E, IntegerOrEnum T>
constexpr E& operator>>= (E& lhs, T rhs) noexcept { return lhs = (E)(std::to_underlying(lhs) >> UnderlyingOrIdentity(rhs)); }

#pragma endregion Assignment operators
