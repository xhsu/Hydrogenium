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


//struct fn_rpl : Linker<fn_rpl, iter_multibytes, dir_backward, cmp_case_ignored, info_u8>
using fn_rpl2_debug = Linker<empty_comp_t, ret_as_marshaled, iter_multibytes, dir_backward, cmp_case_ignored, info_u8>;

template <typename, typename Base>
struct alg_rpl2 : Base
{
	//REQ_TYPE_INFO;
	using typename Base::char_type;
	using typename Base::owner_type;
	using typename Base::view_type;

	//REQ_COMPARATOR;
	using Base::ChEql;
	using Base::ChCmp;

	//REQ_DIR_MGR;
	using Base::is_reverse;

	//REQ_ITER_MGR;
	using Base::Arithmetic;
	using Base::ArithCpy;
	using Base::Get;
	using Base::ValueOf;

	// #COMPILER_BUG if __super were used here, the type will be deduced as <int>
	using value_type = decltype(ValueOf(typename view_type::iterator{}));

	static constexpr ptrdiff_t detail_cnt(view_type const& str) noexcept
	{
		auto const begin = str.begin(), end = str.end();

		ptrdiff_t n{};
		for (auto it = begin; it < end; Arithmetic(it, begin, end, 1)) { ++n; }

		return n;
	}

	static constexpr view_type detail_sub(view_type str, ptrdiff_t count) noexcept
	{
		// the moving iter is already included in this function. No additional loop needed!
		auto [begin, it, end] = __super::Get(str, count);

		if constexpr (!is_reverse)
		{
			return { begin, end };	// in the case of multibyte: begin always pointing to the head of UTF stream, if not reversed.
		}
		else
		{
			auto const fwit1 = ToForwardIter(end, false);	// we are taking last N characters, so including the 'ending' one just defeat the purpose.
			auto const fwed1 = ToForwardIter(begin, false);	// in reverse_iter, rbegin is the actual end.

			return { fwit1, fwed1 };
		}
	}

	static constexpr auto detail_find(view_type const& str, view_type const& substr, size_t pos) noexcept
		-> std::pair<size_t, size_t>
	{
		// Just like the original find in std::string_view
		// Ignoring direction here.
		// Returns the absolute byte-distance.

		if (pos >= str.size() || str.empty())
			return { str.npos, str.npos };

		auto const iSubstrCount = detail_cnt(substr);
		auto const abs_begin = std::addressof(str.front());
		auto const bgn = abs_begin + pos;
		auto const ed = abs_begin + str.size();

		auto it = bgn;
		auto const b2 = substr.begin(), e2 = ArithCpy(b2, b2, substr.end(), MAX_COUNT);

		for (; it < ed; Arithmetic(it, bgn, ed, 1))
		{
			auto s1 = it;
			auto s2 = b2;

			// e1 must be generate every loop.
			// This one is needed for croping CMP range.
			auto const e1 = ArithCpy(it, it, ed, iSubstrCount);

			while (
				s1 < e1 && s2 < e2
				&& ChEql(ValueOf(s1), ValueOf(s2))
				)
			{
				Arithmetic(s1, it, e1, 1);
				Arithmetic(s2, b2, e2, 1);
			}

			// Preventing deducing typing as something like 'int32_t'
			char32_t const c1 = s1 == e1 ? '\0' : ValueOf(s1);
			char32_t const c2 = s2 == e2 ? '\0' : ValueOf(s2);

			if (ChCmp(c1, c2) == 0)
				// 'it' is the begin of the equal sequence, not 's1'
				// Or rather, 's1' is the end of the equal sequence.
				return { static_cast<size_t>(it - abs_begin), static_cast<size_t>(e1 - it), };
		}

		return { str.npos, str.npos };
	}

	constexpr void operator()(owner_type* pstr, view_type from, view_type to, ptrdiff_t count = MAX_COUNT) const noexcept
	{
		if (from.empty())
			return;

		auto const substr = detail_sub(*pstr, count);
		if (substr.empty())
			return;

		owner_type editing{ substr };
		auto const editing_begin = std::addressof(substr.front()) - std::addressof(pstr->front());
		auto const editing_length = substr.length();	// byte-length

		for (auto [start_pos, eql_len] = detail_find(editing, from, 0);
			start_pos != editing.npos;
			std::tie(start_pos, eql_len) = detail_find(editing, from, start_pos))
		{
			editing.replace(start_pos, eql_len, to);
			start_pos += to.length();	// add the new length rather than original length (eql_len)
		}

		pstr->replace(editing_begin, editing_length, editing);
	}

	template <size_t N>
	constexpr void operator()(char_type (&rgsz)[N], view_type from, view_type to, ptrdiff_t count = MAX_COUNT) const noexcept
	{
		owner_type res{ rgsz };
		operator()(&res, from, to, count);

		std::ranges::fill(rgsz, '\0');
		for (auto [lhs, rhs] : std::views::zip(rgsz, res))
			lhs = rhs;

		// If the last cell was overwrited, set it to null terminal.
		if (res.size() >= (N - 1))
			rgsz[N - 1] = '\0';
	}

	constexpr auto operator()(std::same_as<view_type> auto str, view_type from, view_type to, ptrdiff_t count = MAX_COUNT) const noexcept
		-> decltype(Base::Transform(str.begin(), str.end())) requires (StringPolicy::ModifyPostProcessor<Base, char_type>)
	{
		owner_type res{ str };
		operator()(&res, from, to, count);

		return Base::Transform(res.begin(), res.end());
	}
};

using fn_rpl = Linker<empty_comp_t, alg_rpl2, iter_multibytes, dir_backward, cmp_case_ignored, info_u8>;
using fn_rpl2 = Linker<empty_comp_t, alg_rpl2, ret_as_marshaled, iter_multibytes, dir_backward, cmp_case_ignored, info_u8>;

constexpr bool UnitTest_Replace()
{
	fn_rpl MbsIRpl{};

	std::string sz0{ u8"Teßting caße for ßpecial ẞ in German." };
	MbsIRpl(&sz0, u8"ß", u8"s");

	if (sz0 != "Testing case for special s in German.")
		return false;

	sz0 = u8"ẞßẞß";
	MbsIRpl(&sz0, u8"ß", u8"s");
	if (sz0 != "ssss")
		return false;

	sz0 = u8"ßẞßẞ";
	MbsIRpl(&sz0, u8"ß", u8"s");
	if (sz0 != "ssss")
		return false;

	{
		// Additional spaces for expanding usage.
		char rgsz[64]{ "Testing case for special S in German." };
		MbsIRpl(rgsz, "s", u8"ß");

		if (std::string_view{ u8"Teßting caße for ßpecial ß in German." } != rgsz)
			return false;
	}

	{
		char rgsz[]{ u8"Teßting caße for ßpecial ẞ in German." };
		MbsIRpl(rgsz, u8"ß", "S", 12);

		if (std::string_view{ u8"Teßting caße for ßpecial S in German." } != rgsz)
			return false;
	}

	return true;
}
static_assert(UnitTest_Replace());

int main(int, char* []) noexcept
{
	using namespace Hydrogenium::String::UnitTest;

	Timer t0{};

	UnitTest_StrFry();	// Run-time only.
	UnitTest_StrTok();	// Run-time only.

	fn_rpl MbsIRpl{};

	constexpr int64_t TEST_COUNT = 0x100'000;

//	std::latch Start{ 2 }, Finish{ 2 };
	double res1{}, res2{};

	std::string sz0{ u8"Teßting caße for ßpecial ẞ in German." };
	MbsIRpl(&sz0, u8"ß", u8"s");
	assert(sz0 == "Testing case for special s in German.");

	UnitTest_Replace();

	auto Testing1 =
		[&]() noexcept
		{
			std::string sz{};
//			Start.arrive_and_wait();

			auto const time1 = std::chrono::high_resolution_clock::now();

			for (int64_t i = 0; i < TEST_COUNT; ++i)
			{
				sz = u8"Teßting caße for ßpecial ẞ in German.";
				MbsIRpl(&sz, u8"ß", u8"s", 35);
			}

			auto const time2 = std::chrono::high_resolution_clock::now();

			res1 = (time2 - time1).count() / 1e6 / TEST_COUNT;
			fmt::print(u8"Fwd Optimized - {}μs\n", res1);
//			Finish.arrive_and_wait();
		};

	[[maybe_unused]] std::jthread jt{ Testing1 };

	//{
	//	std::string sz{};
	//	Start.arrive_and_wait();

	//	auto const time1 = std::chrono::high_resolution_clock::now();

	//	for (int64_t i = 0; i < TEST_COUNT; ++i)
	//	{
	//		sz = u8"Teßting caße for ßpecial ẞ in German.";
	//		MbsIRpl(StringPolicy::Direction::back_to_front, &sz, u8"ß", u8"s", 35);
	//	}

	//	auto const time2 = std::chrono::high_resolution_clock::now();

	//	res2 = (time2 - time1).count() / 1e6 / TEST_COUNT;
	//	Finish.arrive_and_wait();
	//	fmt::print(u8"Non-Optimized - {}μs\n", res2);
	//}

	//Finish.wait();
	//fmt::print("non-opt/opt == {:.1f}%\n", res2 / res1 * 100.0);

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
