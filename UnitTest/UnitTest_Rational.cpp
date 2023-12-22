//#include <fmt/color.h>
//#include <fmt/ranges.h>

#include <assert.h>

#include <bit>
#include <compare>
#include <concepts>
#include <exception>
#include <limits>

constexpr auto abs_(std::signed_integral auto n) noexcept { return n >= 0 ? n : -n; }
constexpr auto gcd(std::integral auto a, std::integral auto b) noexcept { if (a == 0) return b; return gcd(b % a, a); }
constexpr auto lcm(std::integral auto a, std::integral auto b) noexcept { if (auto const product = a * b; product != 0) return product / gcd(a, b); return 0; }

namespace f32
{
	static_assert(sizeof(float) == sizeof(uint32_t));

	constexpr auto sign(std::same_as<float> auto f) noexcept
	{
		return std::bit_cast<uint32_t>(f) >> 31;
	}

	constexpr auto exponent(std::same_as<float> auto f) noexcept
	{
		//									seeeeeeeemmmmmmmmmmmmmmmmmmmmmmm
		constexpr auto bit_mask = 0b01111111100000000000000000000000;

		return (std::bit_cast<uint32_t>(f) & bit_mask) >> 23;
	}

	constexpr auto mantissa(std::same_as<float> auto f) noexcept
	{
		//									seeeeeeeemmmmmmmmmmmmmmmmmmmmmmm
		constexpr auto bit_mask = 0b00000000011111111111111111111111;

		return (std::bit_cast<uint32_t>(f) & bit_mask);
	}
}

struct rational_t
{
	constexpr void simplify() noexcept
	{
		if (m_numerator == 0)
		{
			m_denominator = 1;
			return;
		}

		if ((m_numerator < 0 && m_denominator < 0)
			|| (m_numerator > 0 && m_denominator < 0)
			)
		{
			m_numerator = -m_numerator;
			m_denominator = -m_denominator;
		}

		if (auto const n = gcd(abs_(m_numerator), abs_(m_denominator)); n > 1)
		{
			m_numerator /= n;
			m_denominator /= n;
		}

		if (m_denominator == 0)
			//throw runtime_error("Denominator cannot be zero for a rational number!");
			std::terminate();
	}

	constexpr rational_t scale_to(std::same_as<int32_t> auto new_deno) const noexcept
	{
		assert(new_deno >= m_denominator);

		auto const scale = new_deno / m_denominator;

		return rational_t{
			m_numerator * scale,
			new_deno,
		};
	}

	constexpr rational_t() noexcept = default;
	constexpr rational_t(std::same_as<int32_t> auto n) noexcept : m_numerator{ n }, m_denominator{ 1 } {}
	constexpr rational_t(std::same_as<int32_t> auto n, std::same_as<int32_t> auto d) noexcept : m_numerator{ n }, m_denominator{ d } { simplify(); }
	constexpr rational_t(std::floating_point auto n) noexcept : rational_t(static_cast<int32_t>(n * 1e9), (int32_t)1e9) {}

	// Unary

	constexpr rational_t operator+() const noexcept { return *this; }
	constexpr rational_t operator-() const noexcept { return rational_t{ -m_numerator, m_denominator, }; }
	constexpr rational_t operator~() const noexcept { return rational_t{ m_denominator, m_numerator }; }
	constexpr rational_t& operator++() noexcept { m_numerator += m_denominator; return *this; }
	constexpr rational_t& operator--() noexcept { m_numerator -= m_denominator; return *this; }

	// Binary

	constexpr rational_t& operator+=(rational_t const& rhs) noexcept
	{
		if (rhs.m_denominator != this->m_denominator)
		{
			auto const lcm_ = lcm(this->m_denominator, rhs.m_denominator);
			auto const lhs_scale = lcm_ / this->m_denominator;
			auto const rhs_scale = lcm_ / rhs.m_denominator;

			assert(lcm_ >= this->m_denominator);
			assert(lcm_ >= rhs.m_denominator);

			this->m_numerator = this->m_numerator * lhs_scale + rhs.m_numerator * rhs_scale;
			this->m_denominator = lcm_;
		}
		else
		{
			this->m_numerator += rhs.m_numerator;
		}

		simplify();
		return *this;
	}
	constexpr rational_t& operator-=(rational_t const& rhs) noexcept { return operator+=(-rhs); }
	constexpr rational_t& operator*=(rational_t const& rhs) noexcept { this->m_numerator *= rhs.m_numerator; this->m_denominator *= rhs.m_denominator; simplify(); return *this; }
	constexpr rational_t& operator/=(rational_t const& rhs) noexcept { return operator*=(~rhs); }
	constexpr rational_t& operator%=(rational_t const& rhs) noexcept { if (this->m_denominator != 1 || rhs.m_denominator != 1) std::terminate(); this->m_numerator %= rhs.m_numerator; return *this; }

	// Conversion

	constexpr explicit operator bool() const noexcept { return m_numerator != 0; }
	constexpr explicit operator double() const noexcept { return (double)m_numerator / (double)m_denominator; }

	// Members

	int32_t m_numerator{ 0 };
	int32_t m_denominator{ 1 };
};

template <>
struct std::numeric_limits<rational_t>
{
	static constexpr rational_t min() noexcept { return rational_t{ 1, std::numeric_limits<int32_t>::max(), }; }
	static constexpr rational_t lowest() noexcept { return rational_t{ std::numeric_limits<int32_t>::lowest(), }; }
	static constexpr rational_t max() noexcept { return rational_t{ std::numeric_limits<int32_t>::max(), }; }
};

constexpr rational_t __fastcall operator+(rational_t const& lhs, rational_t const& rhs) noexcept
{
	if (rhs.m_denominator != lhs.m_denominator)
	{
		auto const lcm_ = lcm(lhs.m_denominator, rhs.m_denominator);
		auto const lhs_scale = lcm_ / lhs.m_denominator;
		auto const rhs_scale = lcm_ / rhs.m_denominator;

		assert(lcm_ >= rhs.m_denominator);
		assert(lcm_ >= lhs.m_denominator);

		return rational_t{
			lhs.m_numerator * lhs_scale + rhs.m_numerator * rhs_scale,
			lcm_,
		};
	}

	return rational_t{
		lhs.m_numerator + rhs.m_numerator,
		lhs.m_denominator,
	};
}
constexpr rational_t __fastcall operator-(rational_t const& lhs, rational_t const& rhs) noexcept { return operator+(lhs, rational_t{ -rhs.m_numerator, rhs.m_denominator }); }
constexpr rational_t __fastcall operator*(rational_t const& lhs, rational_t const& rhs) noexcept { return rational_t{ lhs.m_numerator * rhs.m_numerator, lhs.m_denominator * rhs.m_denominator, }; }
constexpr rational_t __fastcall operator/(rational_t const& lhs, rational_t const& rhs) noexcept { return operator*(lhs, ~rhs); }
constexpr rational_t __fastcall operator%(rational_t const& lhs, rational_t const& rhs) noexcept
{
	if (lhs.m_denominator != 1 || rhs.m_denominator != 1)
		//throw std::runtime_error("Remainder operator can only be used amongst integrals!");
		std::terminate();

	return lhs.m_numerator % rhs.m_numerator;
}

constexpr bool __fastcall operator==(rational_t const& lhs, rational_t const& rhs) noexcept { return lhs.m_numerator == rhs.m_numerator && lhs.m_denominator == rhs.m_denominator; }
constexpr auto __fastcall operator<=>(rational_t const& lhs, rational_t const& rhs) noexcept
{
	auto const lcm_ = lcm(rhs.m_denominator, lhs.m_denominator);
	auto const lhs_scale = lcm_ / lhs.m_denominator;
	auto const rhs_scale = lcm_ / rhs.m_denominator;

	assert(lcm_ >= rhs.m_denominator);
	assert(lcm_ >= lhs.m_denominator);

	return (lhs.m_numerator * lhs_scale) <=> (rhs.m_numerator * rhs_scale);
}

constexpr rational_t __fastcall operator""_r(unsigned long long int n) noexcept { return rational_t{ static_cast<int32_t>(n) }; }
constexpr rational_t __fastcall operator""_r(long double n) noexcept { return rational_t{ n }; }

static_assert(sizeof(rational_t) == sizeof(int32_t) * 2);

inline constexpr auto rat1 = rational_t{ 123, 456 };
static_assert(rat1 == rational_t{ -123, -456 });
static_assert(-rat1 == rational_t{ 123, -456 });
static_assert(+rat1 == rational_t{ 123, 456 });
static_assert(-rat1 == rational_t{ -123, 456 });

inline constexpr auto rat2 = rational_t{ 789, 1011 };
static_assert(rat1 + rat2 == rational_t{ 53793, 51224 });
static_assert(rat1 - rat2 == rational_t{ -26159, 51224 });
static_assert(rat1 * rat2 == rational_t{ 10783, 51224 });
static_assert(rat1 / rat2 == rational_t{ 13817, 39976 });
static_assert(10_r % 3 == 10 % 3_r);
static_assert(10_r / 3 == 10 / 3_r);

static_assert(rat1 < rat2);
static_assert(rat1 <= rat2);
static_assert(rat1 != rat2);
static_assert(rat2 >= rat1);
static_assert(rat2 > rat1);

inline constexpr auto rat3 = rational_t{ 1.048596 };
static_assert(rat3 == 1.048596);
static_assert(rat3 == 1.048596_r);
static_assert(rat3 * 2 == rat3 / 0.5);

inline constexpr auto rat4 = []() consteval
	{
		rational_t ret{};

		for (int32_t i = 1; i < 10; ++i)
			ret += rational_t{ i, i + 1, };

		return ret;
	}
();

inline constexpr auto rat_min = std::numeric_limits<rational_t>::min();
inline constexpr auto rat_lowest = std::numeric_limits<rational_t>::lowest();
inline constexpr auto rat_max = std::numeric_limits<rational_t>::max();