#include "Precompiled.hpp"
#include "UtlString.hpp"
#include "UtlCharConv.hpp"

using namespace Hydrogenium::String::Components;
using namespace Hydrogenium::String;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;


namespace Hydrogenium::String::UnitTest
{
	extern void UnitTest_StrFry();	// Run-time only.
	extern void UnitTest_StrTok();	// Run-time only.
}

extern void UnitTest_Runtime();
extern void UnitTest_UtlString_PerformanceTest() noexcept;



struct Timer final
{
	Timer() noexcept = default;
	Timer(uint32_t count) noexcept : m_count{ count } {}
	~Timer() noexcept
	{
		auto const last =
			std::chrono::high_resolution_clock::now();

		auto const diff = last - m_start;
		auto const sec = diff.count() / 1e9 / m_count;

		if (sec < 1e-8)
			std::println(u8"Average time: {:.3f}ns", diff.count() / (double)m_count);
		else if (sec < 1e-5)
			std::println(u8"Average time: {:.3f}μs", diff.count() / 1e3 / m_count);
		else if (sec < 1e-2)
			std::println(u8"Average time: {:.3f}ms", diff.count() / 1e6 / m_count);
		else
			std::println(u8"Average time: {:.3f}s", diff.count() / 1e9 / m_count);
	}

	std::chrono::high_resolution_clock::time_point m_start{ std::chrono::high_resolution_clock::now() };
	uint32_t m_count{ 1 };
};

template <typename, typename Base>
struct dir_fwd2 : Base
{
	static_assert(!requires{ typename Base::policy_dir; }, "Only one directional policy allowed!");
	using policy_dir = dir_fwd2;

	inline static constexpr bool is_reverse = false;

	static constexpr auto Begin(auto&& r) noexcept -> decltype(std::ranges::data(r))
	{
		return std::ranges::data(r);
	}

	static constexpr auto End(auto&& r) noexcept -> decltype(std::ranges::data(r))
	{
		return std::ranges::data(r) + std::ranges::size(r);
	}
};

template <typename, typename Base>
struct dir_bwd2 : Base
{
	static_assert(!requires{ typename Base::policy_dir; }, "Only one directional policy allowed!");
	using policy_dir = dir_bwd2;

	inline static constexpr bool is_reverse = true;

	static constexpr auto Begin(auto&& r) noexcept
		-> decltype(std::reverse_iterator{ std::ranges::data(r) })
	{
		return std::reverse_iterator{ std::ranges::data(r) + std::ranges::size(r) };
	}

	static constexpr auto End(auto&& r) noexcept
		-> decltype(std::reverse_iterator{ std::ranges::data(r) })
	{
		return std::reverse_iterator{ std::ranges::data(r) };
	}
};

struct fn_rpl : Linker<fn_rpl, iter_multibytes, dir_bwd2, cmp_case_ignored, info_u8>
{
	using value_type = decltype(ValueOf(typename view_type::iterator{}));

	static constexpr ptrdiff_t detail_cnt(view_type const& str) noexcept
	{
		auto const begin = str.begin(), end = str.end();

		ptrdiff_t n{};
		for (auto it = begin; it < end; Arithmetic(it, begin, end, 1)) { ++n; }

		return n;
	}

	static constexpr int detail_cmp(view_type str, view_type const& substr, ptrdiff_t const iSubstrCount) noexcept
	{
		// The comparing length must be the length of substr.
		// The comparison must be performed as forwarding direction.
		auto const b1 = str.begin(), e1 = ArithCpy(b1, b1, str.end(), iSubstrCount);
		auto const b2 = substr.begin(), e2 = ArithCpy(b2, b2, substr.end(), iSubstrCount);
		auto s1 = b1, s2 = b2;

		while (
			s1 < e1 && s2 < e2
			&& ChEql(ValueOf(s1), ValueOf(s2))
			)
		{
			Arithmetic(s1, b1, e1, 1);
			Arithmetic(s2, b2, e2, 1);
		}

		// Preventing deducing as something like 'int32_t'
		value_type const c1 = s1 == e1 ? '\0' : ValueOf(s1);
		value_type const c2 = s2 == e2 ? '\0' : ValueOf(s2);

		return ChCmp(c1, c2);
	}

	static constexpr auto detail_get(owner_type* pstr, ptrdiff_t count) noexcept
	{
		auto [bgn, it, ed] = Get(*pstr, count);
	}

	constexpr void operator()(owner_type* pstr, view_type src, view_type dest, ptrdiff_t count = MAX_COUNT) const noexcept
	{
		if (src.empty() || pstr->empty())
			return;

		bool const bSizeDifference = src.size() != dest.size();
		bool const bStringExpending = src.size() < dest.size();

		auto const src_cnt = detail_cnt(src);
		auto bgn = pstr->begin(), ed = ArithCpy(bgn, bgn, pstr->end(), count);

		for (auto it = bgn; it < ed; /* do nothing */)
		{
			if (detail_cmp({ it, ed }, src, src_cnt) == 0)
			{
				// save offset before iterator invalidation could happen.
				auto const pos = it - bgn;

				// Replacing X amount of graphemes, for case insensitive. (like ẞ and ß)
				pstr->replace(it, ArithCpy(it, bgn, ed, src_cnt), dest);

				// the action replace() potentially invalidates all iterator.
				if (bStringExpending)
				{
					bgn = pstr->begin();
					it = bgn + (pos + std::ssize(dest));	// No ArithCpy() since we need to handle with native size of input string type.
				}
				else
					// In case 'to' contains 'from', like replacing 'x' with 'yx'
					it += std::ssize(dest);

				// endpos will have to be update if the native size of src and dest is different.
				if (bSizeDifference)
					ed = ArithCpy(bgn, bgn, pstr->end(), count);
			}
			else
				Arithmetic(it, bgn, ed, 1);
		}
	}

	constexpr void operator()(StringPolicy::Direction::backwards_t, owner_type* pstr, view_type src, view_type dest, ptrdiff_t count = MAX_COUNT) const noexcept
	{
		if (src.empty() || pstr->empty())
			return;

		bool const bSizeDifference = src.size() != dest.size();
		bool const bSizeExpanding = src.size() < dest.size();
		auto const src_cnt = detail_cnt(src);
		auto const src_len = src.length(), dest_len = dest.length();

		auto [bgn, it, ed] = __super::Get(*pstr, count);

		for (; it < ed; /* do nothing */)
		{
			if (auto const fwit = ToForwardIter(it, it != ed);
				detail_cmp({ fwit, pstr->data() + pstr->size() }, src, src_cnt) == 0)
			{
				// save offset before iterator invalidation could happen.
				auto const pos = fwit - pstr->data();
				auto const pos_rel = it - bgn;

				// std::string has a different set of iterator.
				auto const o_bgn = pstr->data(),
					o_ed = o_bgn + pstr->size();
				auto const rpl_len = ArithCpy(fwit, o_bgn, o_ed, src_cnt) - fwit;

				// Replacing X amount of graphemes, for case insensitive. (like ẞ and ß)
				pstr->replace(pos, rpl_len, dest);

				if (count != MAX_COUNT && bSizeDifference)
					count = count - src_len + dest_len;

				// the action replace() potentially invalidates all iterator.
				//if (o_bgn != pstr->data())
				{
					std::tie(bgn, std::ignore, ed) = Get(*pstr, count);
					it = bgn + (pos_rel + rpl_len);
				}

				if constexpr (is_reverse)
				{
					it = std::reverse_iterator{ pstr->data() + pos };
					Arithmetic(it, bgn, ed, 1);
				}
				else
				{
					//it = pstr->data() + pos + rpl_len;
				}

				// In case 'to' contains 'from', like replacing 'x' with 'yx'
				if constexpr (!is_reverse)
					it += std::ssize(dest);
			}
			else
				Arithmetic(it, bgn, ed, 1);
		}
	}
};

constexpr bool UnitTest_Replace()
{
	fn_rpl MbsIRpl{};

	std::string sz0{ u8"Teßting caße for ßpecial ẞ in German." };
	MbsIRpl(StringPolicy::Direction::back_to_front, &sz0, u8"ß", u8"s");

	if (sz0 != "Testing case for special s in German.")
		return false;

	sz0 = u8"ẞßẞß";
	MbsIRpl(StringPolicy::Direction::back_to_front, &sz0, u8"ß", u8"s");
	if (sz0 != "ssss")
		return false;

	sz0 = u8"ßẞßẞ";
	MbsIRpl(StringPolicy::Direction::back_to_front, &sz0, u8"ß", u8"s");
	if (sz0 != "ssss")
		return false;

	return true;
}
//static_assert(UnitTest_Replace());

int main(int, char* []) noexcept
{
	using namespace Hydrogenium::String::UnitTest;

	Timer t0{};

	UnitTest_StrFry();	// Run-time only.
	UnitTest_StrTok();	// Run-time only.

	fn_rpl MbsIRpl{};

	constexpr int64_t TEST_COUNT = 0x100'000;

	std::latch Start{ 2 }, Finish{ 2 };
	double res1{}, res2{};

	std::string sz0{ u8"Teßting caße for ßpecial ẞ in German." };
	MbsIRpl(StringPolicy::Direction::back_to_front, &sz0, u8"ß", u8"s");
	assert(sz0 == "Testing case for special s in German.");

	UnitTest_Replace();

	auto Testing1 =
		[&]() noexcept
		{
			std::string sz{};
			Start.arrive_and_wait();

			auto const time1 = std::chrono::high_resolution_clock::now();

			for (int64_t i = 0; i < TEST_COUNT; ++i)
			{
				sz = u8"Teßting caße for ßpecial ẞ in German.";
				MbsIRpl(&sz, u8"ß", u8"s", 35);
			}

			auto const time2 = std::chrono::high_resolution_clock::now();

			res1 = (time2 - time1).count() / 1e6 / TEST_COUNT;
			fmt::print(u8"Fwd Optimized - {}μs\n", res1);
			Finish.arrive_and_wait();
		};

	[[maybe_unused]] std::jthread jt{ Testing1 };

	{
		std::string sz{};
		Start.arrive_and_wait();

		auto const time1 = std::chrono::high_resolution_clock::now();

		for (int64_t i = 0; i < TEST_COUNT; ++i)
		{
			sz = u8"Teßting caße for ßpecial ẞ in German.";
			MbsIRpl(StringPolicy::Direction::back_to_front, &sz, u8"ß", u8"s", 35);
		}

		auto const time2 = std::chrono::high_resolution_clock::now();

		res2 = (time2 - time1).count() / 1e6 / TEST_COUNT;
		Finish.arrive_and_wait();
		fmt::print(u8"Non-Optimized - {}μs\n", res2);
	}

	Finish.wait();
	fmt::print("non-opt/opt == {:.1f}%\n", res2 / res1 * 100.0);

	//UnitTest_Runtime();
}

/*
Fwd Optimized - 0.18820117416381835μs
Non-Optimized - 0.1688611222267151μs
non-opt/opt == 89.7%
Average time: 197.357s

Fwd Optimized - 0.06444949893951415μs
Non-Optimized - 0.06908581686019898μs
non-opt/opt == 107.2%
Average time: 72.452s

// Pointer version - release
Fwd Optimized - 0.07568526058197021μs
Non-Optimized - 0.04803600950241089μs
non-opt/opt == 63.5%
Average time: 79.370s
*/
