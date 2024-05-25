/*
	Created at: May 20 2024
*/

#pragma once

#define HYDROGENIUM_UTL_CHARCONV 20240520L

#include <algorithm>
#include <limits>
#include <numeric>
#include <ranges>
#include <string_view>
#include <string>
#include <vector>

#include "detail/UtlDecimal.hpp"

namespace Hydrogenium::detail_charconv
{
	struct rational_t final
	{
		constexpr rational_t() noexcept = default;
		constexpr rational_t(int64_t num, uint64_t dom) noexcept : m_num{ num }, m_dom{ dom } { Simplify(); }

		constexpr void Simplify() noexcept
		{
			auto const d = std::gcd(m_num, m_dom);

			m_num /= d;
			m_dom /= d;
		}

		constexpr bool GreaterThanOrEqualToOne() const noexcept
		{
			return std::cmp_greater_equal(m_num, m_dom);
		}

		constexpr rational_t operator*(int32_t mul) const noexcept
		{
			auto const num = m_num * mul;
			auto const d = std::gcd(num, m_dom);

			return { static_cast<int32_t>(num / d), m_dom / d };
		}
		constexpr rational_t& operator*=(int32_t mul) noexcept
		{
			m_num *= mul;
			Simplify();

			return *this;
		}

		constexpr rational_t operator+(rational_t rhs) const noexcept
		{
			rational_t ret{};

			if (this->m_dom == rhs.m_dom)
			{
				ret.m_num = this->m_num + rhs.m_num;
				ret.m_dom = this->m_dom;
			}
			else
			{
				auto const m = std::lcm(this->m_dom, rhs.m_dom);

				ret.m_num = this->m_num * (m / this->m_dom) + rhs.m_num * (m / rhs.m_dom);
				ret.m_dom = m;
			}

			ret.Simplify();
			return ret;
		}
		constexpr rational_t& operator+=(rational_t rhs) noexcept
		{
			if (this->m_dom == rhs.m_dom)
			{
				this->m_num += rhs.m_num;
			}
			else
			{
				auto const m = std::lcm(this->m_dom, rhs.m_dom);

				this->m_num = this->m_num * (m / this->m_dom) + rhs.m_num * (m / rhs.m_dom);
				this->m_dom = m;
			}

			this->Simplify();
			return *this;
		}

		constexpr rational_t& operator--() noexcept
		{
			m_num -= m_dom;	// #UPDATE_AT_CPP26 sat cast
			return *this;
		}

		constexpr operator double() const noexcept { return (double)m_num / (double)m_dom; }
		explicit constexpr operator bool() const noexcept { return m_num != 0; }

		int64_t m_num{};
		uint64_t m_dom{ 1 };
	};

	constexpr std::string stringfied_float_add(std::string_view lhs, std::string_view rhs)
	{
		auto const dotpos1 = lhs.find_first_of('.') + 1;
		auto const dotpos2 = rhs.find_first_of('.') + 1;

		auto const integral1{ lhs.substr(0, dotpos1 - 1) };
		auto const integral2{ rhs.substr(0, dotpos2 - 1) };
		auto const fraction1{ lhs.substr(dotpos1) };
		auto const fraction2{ rhs.substr(dotpos2) };

		std::string ret{};

		bool carry = false;
		for (auto [l, r] :
			std::views::zip(integral1 | std::views::reverse, integral2 | std::views::reverse))
		{
			auto sum = (l - '0') + (r - '0') + (uint8_t)carry;
			carry = false;

			if (sum >= 10)
			{
				sum -= 10;
				carry = true;
			}

			ret.insert(ret.begin(), static_cast<char>('0' + sum));
		}

		auto const prefixing_integral =
			integral1.size() > integral2.size()
			?
			integral1.substr(0, integral1.size() - integral2.size())
			:
			integral2.substr(0, integral2.size() - integral1.size());

		if (!prefixing_integral.empty())
		{
			ret.insert(0, prefixing_integral);

			if (carry)
			{
				++ret[prefixing_integral.size() - 1];
				carry = false;
			}
		}
		else if (carry)
		{
			ret.insert(0, "1");
			carry = false;
		}

		// just remember it first, insert it later.
		auto const dotpos_ret = ret.size();

		for (auto [l, r] :
			std::views::zip(fraction1, fraction2))
		{
			auto sum = (l - '0') + (r - '0');

			if (sum >= 10)
			{
				ret.back() += 1;
				sum -= 10;
			}

			ret.push_back('0' + sum);
			//fmt::println("{} + {} == {}", l, r, sum);
		}

		carry = false;
		for (auto& c : ret | std::views::reverse)
		{
			if (carry)
			{
				carry = false;
				c += 1;
			}

			if (c > '9')
			{
				c -= 10;
				carry = true;
			}
		}

		// add the decimal point before inserting number in front such invalidate our spot.
		ret.insert(dotpos_ret, ".");

		if (carry)
			ret.insert(0, "1");

		auto const tailing_fraction =
			fraction1.size() < fraction2.size()
			?
			fraction2.substr(fraction1.size())
			:
			fraction1.substr(fraction2.size());

		ret.append_range(tailing_fraction);

		//fmt::println("{}", ret);
		return ret;
	}
}

template <std::integral T = int32_t>
[[nodiscard]]
constexpr T UTIL_strtoi(std::string_view str)
{
	constexpr auto is_digit =
		[](auto c) noexcept -> bool
		{
			return '0' <= c && c <= '9';
		};

	T acc = 0;
	bool neg = false;
	auto it = str.begin();
	auto const ed = str.end();

	if (it != ed && *it == '-')
	{
		++it;
		neg = true;
	}

	for (; it != ed && is_digit(*it); ++it)
	{
		acc *= 10;
		acc += *it - 0x30;
	}

	return neg ? -acc : acc;
}

template <typename T>
[[nodiscard]]
constexpr std::string UTIL_strfromi(T num) noexcept
{
	std::string ret{};
	ret.reserve(32);

	if constexpr (std::is_signed_v<T>)
	{
		if (std::cmp_less(num, 0))
		{
			ret.push_back('-');
			num = -num;	// #UPDATE_AT_CPP26 sat cast

			if (std::cmp_less(num, 0)) [[unlikely]]
				num = std::numeric_limits<T>::max();
		}
	}

	auto const fn =
		[&](this auto&& self, T num) noexcept -> void
		{
			if (num / 10 > 0)
				self(num / 10);	// Discard the tailing digit.

			ret.push_back(static_cast<char>('0' + num % 10));
		};

	fn(num);
	return ret;
}

static_assert(UTIL_strfromi(12345) == "12345");
static_assert(UTIL_strfromi(-12345) == "-12345");

[[nodiscard]]
constexpr double UTIL_strtof(std::string_view dec) noexcept
{
	using namespace Hydrogenium::detail_charconv;

	constexpr auto TenToThePowerOf =
		[](uint8_t count) /*#UPDATE_AT_CPP23 static*/ noexcept -> uint32_t
		{
			uint32_t ret{ 1 };
			for (uint8_t i = 0; i < count; ++i)
				ret *= 10;

			return ret;
		};

	// Basic type info: double
#define DBL_MANTISSA 52
#define DBL_EXPONENT 11
#define DBL_BIAS 1023
	static_assert(sizeof(double) == sizeof(uint64_t), "Unsupported platform.");

	// Step I. Split integral and fraction, and make them binary.

	std::string_view Integral, Fraction;

	uint8_t const neg = dec.starts_with('-');
	if (neg)
		dec = dec.substr(1);

	auto const pos = dec.find_first_of('.');
	if (pos != std::string_view::npos)
	{
		Integral = dec.substr(0, pos);
		Fraction = dec.substr(pos + 1);
	}
	else
		Integral = dec;

	if (pos == 0)
		Integral = "0";
	else if (pos == dec.size() - 1)
		Fraction = "";

	if ((Integral == "0" || Integral.empty()) && Fraction.empty())
		return 0;

	auto iIntegral = UTIL_strtoi(Integral), iIntegralPartDigits = 0;
	auto flFraction = rational_t{ UTIL_strtoi(Fraction), TenToThePowerOf((uint8_t)Fraction.size()) };
	std::vector<bool> Binary{};	// std::dynamic_bitset, I know what I am doing.

	while (iIntegral)
	{
		auto const rem = iIntegral % 2;
		Binary.push_back((bool)rem);
		iIntegral /= 2;
		++iIntegralPartDigits;
	}

	// by definition, this should be read reversely
	std::ranges::reverse(Binary);

	// Fraction part
	while (flFraction && Binary.size() < 80)
	{
		flFraction *= 2;

		if (flFraction.GreaterThanOrEqualToOne())
		{
			--flFraction;
			Binary.push_back(true);
		}
		else
			Binary.push_back(false);
	}

	if (Binary.size() < 80)
		Binary.append_range(std::views::repeat(false) | std::views::take(80 - Binary.size()));

	// Step II. Convert to scientific notation

	auto const first_one = std::ranges::find(Binary, true);
	uint64_t iMantissa{};	// out
	uint16_t bias_exp{};	// out

	if (first_one != Binary.end())
	{
		auto const dotpos = (ptrdiff_t)(first_one - Binary.begin()) + 1;	// AFTER the first '1'
		auto const exponent = iIntegralPartDigits - dotpos;	// how many times we have the decimal point moved?
		bias_exp = static_cast<decltype(bias_exp)>(DBL_BIAS + exponent);

		std::ranges::subrange Mantissa{ Binary.begin() + dotpos, Binary.end() };
		for (auto&& b : Mantissa | std::views::take(DBL_MANTISSA))
		{
			iMantissa <<= 1;
			iMantissa |= (decltype(iMantissa))b;
		}

		// "round" up
		if (Mantissa[DBL_MANTISSA] == true)
			iMantissa += 0b1;
	}
	else
		return 0;

	/// Step III. Assemble all informations we have.

		// This is for float32.
		//std::array<uint8_t, 4> bytes{};
		//bytes[0] = (neg << 7) | ((bias_exp & 0xFF) >> 1);
		//bytes[1] = ((bias_exp & 0b1) << 7) | (iMantissa >> (23 - 7));
		//bytes[2] = static_cast<uint8_t>(iMantissa >> (23 - 7 - 8));
		//bytes[3] = static_cast<uint8_t>(iMantissa >> (23 - 7 - 16));

	uint64_t const res =
		(uint64_t)neg << (DBL_EXPONENT + DBL_MANTISSA) | uint64_t(bias_exp & 0b111'1111'1111) << DBL_MANTISSA | iMantissa;

	return std::bit_cast<double>(res);

#undef DBL_MANTISSA
#undef DBL_EXPONENT
}

static_assert(UTIL_strtof("") == 0.f && UTIL_strtof("0") == 0.f && UTIL_strtof("0.0") == 0.f);	// purpose: testing null input.
static_assert(UTIL_strtof("9527") == 9527.f);	// purpose: testing num without fraction part.
static_assert(UTIL_strtof("0.15625") == 0.15625f);	// purpose: testing num without integral part.
static_assert(UTIL_strtof("263.3") == 263.3);	// purpose: testing inf loop.
static_assert(UTIL_strtof("0.1") == 0.1);	// purpose: testing round up.

[[nodiscard]]
constexpr std::string UTIL_strfromf(double fl) noexcept
{
	using namespace Hydrogenium::detail_charconv;

	auto const hex = std::bit_cast<uint64_t>(fl);
	auto const sign = hex >> 63;
	auto const exponent = int16_t((hex >> 52) & 0x7FF);	// bias stayed.
	auto const mantissa = (hex & 0xF'FFFF'FFFF'FFFFull) | (0b1ull << 52);	// preceeding 1 added.

	std::string ret{ "0.0" };

	int16_t i = exponent - 52;
	uint8_t counter = 0;
	uint64_t frac = mantissa;

	for (; counter < 53;
		++i, ++counter, frac >>= 1)
	{
		if (frac & 0b1 && i >= 0)
			ret = stringfied_float_add(ret, DBL_TABLE[i]);
	}

	return ret;
}
