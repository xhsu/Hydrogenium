#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace std;
using namespace Hydrogenium::String;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;


namespace Hydrogenium::String::UnitTest
{
	extern void UnitTest_StrFry();	// Run-time only.
	extern void UnitTest_StrTok();	// Run-time only.
}

extern void UnitTest_Runtime();



constexpr int32_t UTIL_atoi(std::string_view str)
{
	constexpr auto is_digit =
		[](auto c) noexcept -> bool
		{
			return '0' <= c && c <= '9';
		};

	int32_t acc = 0;
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

constexpr double UTIL_atof(const char* s) noexcept
{
	constexpr auto is_digit =
		[](auto c) noexcept
		{
			return '0' <= c && c <= '9';
		};

	// This function stolen from either Rolf Neugebauer or Andrew Tolmach. 
	// Probably Rolf.
	double a{};
	int32_t e{}, c{};
	bool neg = false;

	if (s && *s == '-')
	{
		++s;
		neg = true;
	}

	while ((c = *s++) != '\0' && is_digit(c))
	{
		a = a * 10.0 + (c - '0');
	}

	if (c == '.')
	{
		while ((c = *s++) != '\0' && is_digit(c))
		{
			a = a * 10.0 + (c - '0');
			e = e - 1;
		}
	}

	if (c == 'e' || c == 'E')
	{
		int32_t sign = 1;
		int32_t i = 0;

		c = *s++;

		if (c == '+')
		{
			c = *s++;
		}
		else if (c == '-')
		{
			c = *s++;
			sign = -1;
		}

		while (is_digit(c))
		{
			i = i * 10 + (c - '0');
			c = *s++;
		}
		e += i * sign;
	}

	while (e > 0)
	{
		a *= 10.0;
		--e;
	}

	while (e < 0)
	{
		a *= 0.1;
		++e;
	}

	return neg ? -a : a;
}

constexpr uint32_t UTIL_Pow(uint8_t count) noexcept
{
	int32_t ret{ 1 };
	for (uint8_t i = 0; i < count; ++i)
		ret *= 10;

	return ret;
}

struct rational_t final
{
	constexpr void Rationalize() noexcept
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
		Rationalize();

		return *this;
	}

	constexpr rational_t& operator--() noexcept
	{
		m_num -= m_dom;	// #UPDATE_AT_CPP26 sat cast
		return *this;
	}

	constexpr operator double() const noexcept { return (double)m_num / (double)m_dom; }
	explicit constexpr operator bool() const noexcept { return m_num != 0; }

	int32_t m_num{};
	uint32_t m_dom{ 1 };
};


constexpr float UTIL_strtof(std::string_view dec) noexcept
{
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

	auto iIntegral = UTIL_atoi(Integral);
	auto flFraction = rational_t{ UTIL_atoi(Fraction), UTIL_Pow((uint8_t)Fraction.size()) };
	std::string IntegralBinary{};

	while (iIntegral)
	{
		auto const rem = iIntegral % 2;
		IntegralBinary.push_back(rem + '0');
		iIntegral /= 2;
	}

	// by definition, this should be read reversely
	std::ranges::reverse(IntegralBinary);

	// Fraction part
	std::string FractionBinary{};

	while (flFraction && FractionBinary.size() < 64)
	{
		flFraction *= 2;

		if (flFraction.GreaterThanOrEqualToOne())
		{
			--flFraction;
			FractionBinary.push_back('1');
		}
		else
			FractionBinary.push_back('0');
	}

	if (FractionBinary.size() < 64)
		FractionBinary += std::string(64 - FractionBinary.size(), '0');

// Step II. Convert to scientific notation

	uint32_t iMantissa{}, bias_exp{};
	std::string WholeBinary = IntegralBinary + FractionBinary;
	auto const first_one = WholeBinary.find_first_of('1');

	if (first_one != std::string::npos)
	{
		auto const dotpos = (ptrdiff_t)first_one + 1;	// AFTER the first '1'
		auto const expn = std::ssize(IntegralBinary) - dotpos;
		bias_exp = static_cast<uint32_t>(127 + expn);

		auto const Mantissa = std::string_view{ WholeBinary.begin() + dotpos, WholeBinary.end() };
		for (auto&& c : Mantissa | std::views::take(23))
		{
			iMantissa <<= 1;
			iMantissa |= (c - '0');
		}

		// "round" up
		if (Mantissa[23] == '1')
			iMantissa |= 0b1;
	}
	else
		return 0;

/// Step III

	std::array<uint8_t, 4> bytes{};
	bytes[0] = (neg << 7) | ((bias_exp & 0xFF) >> 1);
	bytes[1] = ((bias_exp & 0b1) << 7) | (iMantissa >> (23 - 7));
	bytes[2] = static_cast<uint8_t>(iMantissa >> (23 - 7 - 8));
	bytes[3] = static_cast<uint8_t>(iMantissa >> (23 - 7 - 16));

	uint32_t const res
		= (bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3];

	return std::bit_cast<float>(res);
}

static_assert(UTIL_strtof("") == 0.f && UTIL_strtof("0") == 0.f && UTIL_strtof("0.0") == 0.f);	// purpose: testing null input.
static_assert(UTIL_strtof("9527") == 9527.f);	// purpose: testing num without fraction part.
static_assert(UTIL_strtof("0.15625") == 0.15625f);	// purpose: testing num without integral part.
static_assert(UTIL_strtof("263.3") == 263.3f);	// purpose: testing inf loop.
static_assert(UTIL_strtof("0.1") == 0.1f);	// purpose: testing round up.


int main(int, char* []) noexcept
{
	using namespace Hydrogenium::String::UnitTest;

	UnitTest_StrFry();	// Run-time only.
	UnitTest_StrTok();	// Run-time only.

	//UnitTest_Runtime();
	constexpr auto r = sizeof(long double);
}
