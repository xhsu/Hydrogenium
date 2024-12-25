#include <stdint.h>
#include <assert.h>

#include <algorithm>
#include <bit>
#include <print>
#include <ranges>
#include <tuple>
#include <vector>

#include <charconv>
#include <string_view>

constexpr auto full_adder(uint8_t A, uint8_t B, uint8_t C = 0) noexcept
{
	return std::pair<uint8_t, uint8_t>{
		C ^ (A^ B),					// Sum
			(A& B) | (B & C) | (A & C),	// Carry
	};
}

static_assert(full_adder(0, 0) == std::pair{ 0, 0 });
static_assert(full_adder(1, 0) == std::pair{ 1, 0 });
static_assert(full_adder(0, 1) == std::pair{ 1, 0 });
static_assert(full_adder(1, 1) == std::pair{ 0, 1 });
static_assert(full_adder(0, 0, 1) == std::pair{ 1, 0 });
static_assert(full_adder(1, 0, 1) == std::pair{ 0, 1 });
static_assert(full_adder(0, 1, 1) == std::pair{ 0, 1 });
static_assert(full_adder(1, 1, 1) == std::pair{ 1, 1 });

template <std::integral T>
constexpr bool bit_at(T num, T idx) noexcept
{
	return static_cast<bool>((num >> idx) & 1);
}

constexpr uint8_t bit_at(std::span<uint8_t const> num, size_t idx) noexcept
{
	auto const quo = idx / 8u;
	auto const rem = idx % 8u;

	if (quo >= num.size()) [[unlikely]]
		std::abort();

		return bit_at<uint8_t>(num[quo], (uint8_t)rem);
}

constexpr void bit_assign(std::vector<uint8_t>* num, size_t idx, bool val) noexcept
{
	auto const quo = idx / 8u;
	auto const rem = idx % 8u;

	if (num->size() <= quo)
		num->resize(quo + 1, 0);

	auto const byt = static_cast<uint8_t>(1 << rem);

	if (val)
		num->at(quo) |= byt;
	else
		num->at(quo) &= ~byt;
}

static void UnitTest_BitAssign() noexcept
{
	std::vector<uint8_t> num{ std::from_range, std::bit_cast<std::array<uint8_t, 2>>((uint16_t)321) };
	bit_assign(&num, 20, true);
	bit_assign(&num, 6, false);

	assert(num.size() == 3);

	num.push_back(0);

	assert(*reinterpret_cast<uint32_t*>(num.data()) == 1'048'833);
}

[[nodiscard]]
constexpr size_t bit_capacity(std::span<uint8_t const> num) noexcept
{
	return
		sizeof(std::ranges::range_value_t<decltype(num)>)
		* (size_t)8
		* std::size(num);
}

[[nodiscard]]
constexpr size_t bit_size(std::span<uint8_t const> num) noexcept
{
	if (num.empty())
		return 0;

	auto const r = std::ranges::find_last_if(num, [](auto&& a) { return a != 0; });
	auto const last_byte = r.empty() ? num.back() : *r.begin();

	constexpr auto fn =
		[](uint8_t n) noexcept -> size_t
		{
			for (int8_t i = 7; i >= 0; --i)
			{
				auto const lsh = static_cast<uint8_t>(1 << i);
				if ((n & lsh) == lsh)
					return (size_t)(i + 1);
			}

			return 0;
		};

	static_assert(fn(0b0000'0000) == 0);
	static_assert(fn(0b0000'1111) == 4);
	static_assert(fn(0b0100'0111) == 7);

	return
		sizeof(std::ranges::range_value_t<decltype(num)>)
		* (size_t)8
		* (num.size() - r.size())
		+ fn(last_byte)
		;
}

static void UnitTest_BitCounter() noexcept
{
	constexpr auto num1 = std::bit_cast<std::array<uint8_t, 2>>((uint16_t)15);
	static_assert(bit_capacity(num1) == 16);
	static_assert(bit_size(num1) == 4);

	constexpr auto num2 = std::bit_cast<std::array<uint8_t, 2>>((uint16_t)9527);
	static_assert(bit_capacity(num2) == 16);
	static_assert(bit_size(num2) == 14);
}

constexpr void bit_shrink_to_fit(std::vector<uint8_t>* num) noexcept
{
	for (; !num->empty() && num->back() == 0;)
	{
		num->pop_back();
	}
}

constexpr auto byte_adder(uint8_t A, uint8_t B, uint8_t C = 0) noexcept
{
	uint8_t result{}, carry{ C }, output{};
	for (uint8_t i = 0; i < 8; ++i)
	{
		auto const a = bit_at(A, i);
		auto const b = bit_at(B, i);
		std::tie(result, carry) = full_adder(a, b, carry);
		output |= result << i;
	}

	return std::pair{ output, carry };
}

constexpr bool UnitTest_ByteAdder() noexcept
{
	auto prod = std::views::cartesian_product(
		std::views::iota(0) | std::views::take(256),
		std::views::iota(0) | std::views::take(256),
		std::views::iota(0) | std::views::take(2)
	);	// Exceeds constexpr limit for MSVC (1048576)

	for (auto&& [i, j, k] : prod)
	{
		auto const math = i + j + k;
		auto const sum = math % 256;
		auto const carry = math > 255;

		auto&& [s, c] = byte_adder(i, j, k);

		if (std::is_constant_evaluated())
		{
			if (s != sum || c != (uint8_t)carry)
				return false;
		}
		else
		{
			assert(s == sum);
			assert(c == (uint8_t)carry);
		}
	}

	return true;
}

[[nodiscard]]
constexpr auto word_adder(std::span<uint8_t const> A, std::span<uint8_t const> B, uint8_t C = 0) noexcept
{
	auto const iMaxSize = std::ranges::max(A.size(), B.size());

	std::vector<uint8_t> ret{};
	ret.reserve(iMaxSize + C);

	for (size_t i = 0; i < iMaxSize; ++i)
	{
		if (i < A.size() && i < B.size())
			std::tie(ret.emplace_back(), C) = byte_adder(A[i], B[i], C);
		else if (i < A.size())
			std::tie(ret.emplace_back(), C) = byte_adder(A[i], 0, C);
		else if (i < B.size())
			std::tie(ret.emplace_back(), C) = byte_adder(0, B[i], C);
		else
			std::unreachable();
	}

	if (C)
		ret.emplace_back(C);	// Add only 1.

	return ret;
}

constexpr void word_adder(std::vector<uint8_t>* A, std::span<uint8_t const> B, uint8_t C = 0) noexcept
{
	auto const iMaxSize = std::ranges::max(A->size(), B.size());
	A->reserve(iMaxSize + C);

	for (size_t i = 0; i < iMaxSize; ++i)
	{
		if (i < A->size() && i < B.size())
			std::tie(A->at(i), C) = byte_adder(A->at(i), B[i], C);
		else if (i < A->size())
			std::tie(A->at(i), C) = byte_adder(A->at(i), 0, C);
		else if (i < B.size())
			std::tie(A->emplace_back(), C) = byte_adder(0, B[i], C);
		else
			std::unreachable();
	}

	if (C)
		A->emplace_back(C);	// Add only 1.
}

constexpr void UnitTest_WordAdder() noexcept
{
	constexpr uint8_t u16_A = 128;
	constexpr uint8_t u16_B = 192;
	constexpr uint16_t u16_SUM = u16_A + u16_B;

	constexpr auto u16_A_arr = std::bit_cast<std::array<uint8_t, 1>>(u16_A);
	constexpr auto u16_B_arr = std::bit_cast<std::array<uint8_t, 1>>(u16_B);
	constexpr auto u16_SUM_arr = std::bit_cast<std::array<uint8_t, 2>>(u16_SUM);

	static_assert(std::ranges::equal(u16_SUM_arr, word_adder(u16_A_arr, u16_B_arr)));
	static_assert(bit_at(u16_SUM_arr, 9) == 0);
	static_assert(bit_at(u16_SUM_arr, 8) == 1);
	static_assert(bit_at(u16_SUM_arr, 7) == 0);
}

constexpr void word_lsh(std::vector<uint8_t>* num, uint8_t step) noexcept
{
	std::vector<uint8_t> res{};
	res.reserve(num->capacity() + step / 8u);

	for (size_t i = 0; i < num->size() * 8; ++i)
		bit_assign(&res, i + step, bit_at(*num, i));

	std::swap(res, *num);
}

[[nodiscard]]
constexpr auto word_lsh(std::span<uint8_t const> num, uint8_t step) noexcept -> std::vector<uint8_t>
{
	std::vector<uint8_t> res{};
	res.reserve(num.size() + step / 8u);

	for (size_t i = 0; i < num.size() * 8; ++i)
		bit_assign(&res, i + step, bit_at(num, i));

	return res;
}

constexpr void UnitTest_LSh() noexcept
{
	constexpr auto fn =
		[]() noexcept
		{
			std::vector<uint8_t> num{ std::from_range, std::bit_cast<std::array<uint8_t, 2>>((uint16_t)321) };
			word_lsh(&num, 8);
			num.push_back(0);

			return std::ranges::equal(num, std::bit_cast<std::array<uint8_t, 4>>(82'176));
		};

	static_assert(fn());
}

// Not working.
constexpr auto chars_to_bigint(std::string_view str) noexcept
{
	std::vector<uint8_t> ret{};
	ret.reserve(sizeof(uint64_t) * 2);

	// Max parse-able digits is 19, "9999999999999999999"

	for (auto&& seg : str | std::views::chunk(19))
	{
#ifdef _DEBUG
		std::string_view const VIEW{ seg };
#endif

		uint64_t res{};
		[[maybe_unused]] auto const cvt =
			std::from_chars(seg.data(), seg.data() + seg.size(), res);
		assert(cvt.ec == std::errc{});	// no conversion failure

		// WRONG!
		ret.insert_range(ret.begin(), std::bit_cast<std::array<uint8_t, 8>>(res));
	}

	return ret;
}

template <std::integral T>
constexpr auto int_to_bigint(T val) noexcept
{
	return std::vector<uint8_t>(
		std::from_range,
		std::bit_cast<std::array<uint8_t, sizeof(T)>>(val)
	);
}

[[nodiscard]]
constexpr auto word_multiplier(std::span<uint8_t const> lhs, std::span<uint8_t const> rhs) noexcept -> std::vector<uint8_t>
{
	// lhs: each bit serve as a switch of the 8 bit output.
	// rhs: keep lsh, the result was passed to the switch and add with prev

	auto const lhs_size = bit_size(lhs);
	std::vector<uint8_t> last_res{}, final_ans{};
	for (uint8_t i = 0; i < lhs_size; ++i)
	{
		if (bit_at(lhs, i))
		{
			last_res = word_lsh(rhs, i);
		}
		else
			last_res.clear();

		word_adder(&final_ans, last_res);
	}

	return final_ans;
}

int main(int, char*[]) noexcept
{
	UnitTest_BitCounter();
	UnitTest_BitAssign();
	UnitTest_ByteAdder();
	UnitTest_WordAdder();

	std::vector<uint8_t> num1{ std::from_range, std::bit_cast<std::array<uint8_t, 2>>((uint16_t)1024) };
	std::vector<uint8_t> num2{ std::from_range, std::bit_cast<std::array<uint8_t, 2>>((uint16_t)768) };
	auto const res = word_multiplier(num1, num2);
	auto const rep = *reinterpret_cast<int32_t const*>(res.data());
}
