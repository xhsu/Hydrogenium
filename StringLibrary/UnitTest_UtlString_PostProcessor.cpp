#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::String::Functors::Components;
using namespace Hydrogenium::String::Functors;
using namespace Hydrogenium::StringPolicy::Result;
using namespace Hydrogenium::StringPolicy;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;

using std::string_view;

/*
In middle, forward
	index == 5
	0123456789*
	└────┘

	position == 5
	0123456789*
	└────┘

	view == 56789
	0123456789*
		 └───┴
*/
static constexpr bool UnitTest_RelPos(std::string_view const& str = CJK_NUMBERS_FWD_U8) noexcept
{
	auto const abs_begin = str.begin(), abs_end = str.end();
	auto const [rel_begin, it_, rel_end] =
		Iterating::as_multibytes_t::Get(str, Direction::front_to_back, 0xFF);

	ptrdiff_t n = 0;
	for (auto it = it_; it < rel_end;
		Iterating::as_multibytes.Arithmetic(it, rel_begin, rel_end, 1), ++n)
	{
		auto r = as_position_t::Transform(
			abs_begin, abs_end,
			rel_begin, it, rel_end,
			Iterating::as_multibytes
		);

		if (r != n)
			return false;
	}

	// Reverse test

	auto const [rel_rbegin, rit_, rel_rend] =
		Iterating::as_multibytes.Get(str, Direction::back_to_front, 0xFF);

	n = -1;
	for (auto it = rit_; it < rel_rend;
		Iterating::as_multibytes.Arithmetic(it, rel_rbegin, rel_rend, 1), --n)
	{
		auto r = as_position_t::Transform(
			abs_begin, abs_end,
			rel_rbegin, it, rel_rend,
			Iterating::as_multibytes
		);

		if (r != n)
			return false;
	}

	return true;
}
static_assert(UnitTest_RelPos());
/*
In middle, forward, capped
	index == 5
		 *
	0123456789*
	└────┘

	position == 5
		 *
	0123456789*
	└────┘

	view == 5
		 *
	0123456789*
		 ┴

In middle, backward
	index == 5
	*0123456789
	 └────┘

	position == 4
	*0123456789
		  └───┘

	view == 56789
	*0123456789
		  └───┴

In middle, backward, capped
	index == 5
		  *
	*0123456789
	 └────┘

	position == 4
		  *
	*0123456789
		  └───┘

	view == 56789
		  *
	*0123456789
		  └───┴

No found, forward
	index == 10
	0123456789*
	└─────────┘

	position == 10
	0123456789*
	└─────────┘

	view == null
	0123456789*
			  ┴

No found, backward
	index == 10
	*0123456789
	└─────────┘

	position == 10
	*0123456789
	└─────────┘

	view == null
	*0123456789
	┴

┌─┐
│ ┼
└─┘

├─┤

┬
│
┴
*/

namespace Hydrogenium::StringPolicy::UnitTest
{
	static_assert(requires(char* p) { { as_pointer_t::Transform(p, p, p, p, p, nullptr) } -> std::same_as<char*>; });
	static_assert(as_pointer_t::UnitTestInvoke(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.end()) == &ASCII_NUMBERS_FWD[0]);
	static_assert(as_pointer_t::UnitTestInvoke(ASCII_NUMBERS_FWD.begin() + 9, ASCII_NUMBERS_FWD.end()) == &ASCII_NUMBERS_FWD[9]);
	static_assert(as_pointer_t::UnitTestInvoke(ASCII_NUMBERS_FWD.begin() + 10, ASCII_NUMBERS_FWD.end()) == nullptr);
	static_assert(as_pointer_t::UnitTestInvoke((char*)nullptr, nullptr) == nullptr);

	// Requirement: Transform the returning iter pos into Hydrogenium::UtfAt() compatible input.
	static_assert(requires(char* p) { { as_position_t::Transform(p, p, p, p, p, Iterating::as_multibytes) } -> std::same_as<std::ptrdiff_t>; });
	static_assert(as_position_t::UnitTestInvoke(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.begin() + 5, ASCII_NUMBERS_FWD.end(), Iterating::as_regular_ptr) == 5 && ASCII_NUMBERS_FWD.substr(5)[0] == ASCII_NUMBERS_FWD[5]);
	static_assert(as_position_t::UnitTestInvoke(ASCII_NUMBERS_FWD.rbegin(), ASCII_NUMBERS_FWD.rbegin() + 5, ASCII_NUMBERS_FWD.rend(), Iterating::as_regular_ptr) == -6);
	static_assert(as_position_t::UnitTestInvoke(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.begin() + 10, ASCII_NUMBERS_FWD.end(), Iterating::as_regular_ptr) == 10);	// no found: return strcnt() equivlent.
	static_assert(as_position_t::UnitTestInvoke(CJK_NUMBERS_FWD_U8.begin(), CJK_NUMBERS_FWD_U8.begin() + (9 * 3), CJK_NUMBERS_FWD_U8.end(), Iterating::as_regular_ptr) == (9 * 3));
	static_assert(as_position_t::UnitTestInvoke(CJK_NUMBERS_FWD_U8.begin(), CJK_NUMBERS_FWD_U8.begin() + (9 * 3), CJK_NUMBERS_FWD_U8.end(), Iterating::as_multibytes) == 9);
	static_assert(as_position_t::UnitTestInvoke(CJK_NUMBERS_FWD_U8.begin(), CJK_NUMBERS_FWD_U8.begin() + (10 * 3), CJK_NUMBERS_FWD_U8.end(), Iterating::as_multibytes) == 10);	// no found: return strcnt() equivlent.

	template <typename I, typename D>
	constexpr bool UnitTest_AsPosition(auto&& STR, ptrdiff_t GRAPHEME_COUNT = 10)
	{
		auto const [rel_begin, it, rel_end] = I::Get(STR, D{}, 0xFFFF);

		if (auto const pos = as_position_t::UnitTestInvoke(rel_begin, rel_end, rel_end, I{}); pos != GRAPHEME_COUNT && pos != -(GRAPHEME_COUNT + 1))
			return false;

		for (auto i = it; i < rel_end; I::Arithmetic(i, rel_begin, rel_end, 1))
		{
			if (auto const pos = as_position_t::UnitTestInvoke(rel_begin, i, rel_end, I{}); i < rel_end)
			{
				auto const lhs = I::ValueOf(i);
				auto const rhs = UtfAt(STR, pos);	// The whole purpose of this transformation is to use with UtfAt().

				if (lhs != rhs)
					return false;
			}
		}

		return true;
	}
	static_assert(UnitTest_AsPosition<Iterating::as_multibytes_t, Direction::forwards_t>(ASCII_NUMBERS_FWD));
	static_assert(UnitTest_AsPosition<Iterating::as_multibytes_t, Direction::backwards_t>(ASCII_NUMBERS_FWD));
	static_assert(UnitTest_AsPosition<Iterating::as_multibytes_t, Direction::forwards_t>(CJK_NUMBERS_FWD_U8));
	static_assert(UnitTest_AsPosition<Iterating::as_multibytes_t, Direction::backwards_t>(CJK_NUMBERS_FWD_U8));
	static_assert(UnitTest_AsPosition<Iterating::as_normal_ptr_t, Direction::forwards_t>(RMN_NUMBERS_FWD_W));
	static_assert(UnitTest_AsPosition<Iterating::as_normal_ptr_t, Direction::backwards_t>(RMN_NUMBERS_FWD_W));

	static_assert(requires(char* p) { { as_view_t::Transform(p, p, p, p, p, nullptr) } -> std::same_as<string_view>; });
	static_assert(as_view_t::UnitTestInvoke(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.begin() + 9, ASCII_NUMBERS_FWD.end()) == ASCII_NUMBERS_FWD.substr(9));
	static_assert(as_view_t::UnitTestInvoke(ASCII_NUMBERS_FWD.rbegin(), ASCII_NUMBERS_FWD.rbegin() + 8, ASCII_NUMBERS_FWD.rend()) == ASCII_NUMBERS_FWD.substr(1));

	static constexpr bool UnitTest_Unmanaged() noexcept	// requires C++20 constexpr new.
	{
		auto [cn_txt_abs_begin, cn_txt_it, cn_txt_end]
			= Iterating::as_multibytes.Get(CJK_NUMBERS_FWD_U8, Direction::back_to_front, 100);

		auto const cpy1 = as_unmanaged_t::Transform(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.end(), Iterating::as_regular_ptr);
		auto const cpy2 = as_unmanaged_t::Transform(RMN_NUMBERS_FWD_W.rbegin(), RMN_NUMBERS_FWD_W.rend(), Iterating::as_regular_ptr);
		auto const cpy3 = as_unmanaged_t::Transform(cn_txt_it, cn_txt_end, Iterating::as_multibytes);

		decltype(ASCII_NUMBERS_FWD) const view1{ cpy1 };
		decltype(RMN_NUMBERS_FWD_W) const view2{ cpy2 };
		decltype(CJK_NUMBERS_FWD_U8) const view3{ cpy3 };

		bool const bResult =
			view1 == ASCII_NUMBERS_FWD
			&& view2 == RMN_NUMBERS_BWD_W
			&& Mbs::Cmp(view3, CJK_NUMBERS_BWD_U8) == 0	// #MSVC_BUGGED_compile_time_utf8
			;

		delete[] cpy1;
		delete[] cpy2;
		delete[] cpy3;

		return bResult;
	}
	static_assert(UnitTest_Unmanaged());

	static constexpr bool UnitTest_Marshaled() noexcept
	{
		auto [cn_txt_abs_begin, cn_txt_it, cn_txt_end]
			= Iterating::as_multibytes.Get(CJK_NUMBERS_FWD_U8, Direction::back_to_front, 5);

		auto cpy1 = as_marshaled_t::Transform(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.begin() + 5, Iterating::as_regular_ptr);
		auto cpy2 = as_marshaled_t::Transform(RMN_NUMBERS_FWD_W.rbegin(), RMN_NUMBERS_FWD_W.rbegin() + 5, Iterating::as_regular_ptr);
		auto cpy3 = as_marshaled_t::Transform(cn_txt_it, cn_txt_end, Iterating::as_multibytes);

		bool const bResult =
			ASCII_NUMBERS_FWD.starts_with(cpy1)
			&& RMN_NUMBERS_BWD_W.starts_with(cpy2)
			&& Mbs::Cmp(cpy3, CJK_NUMBERS_BWD_U8, 5) == 0
			;

		return bResult;
	}
	static_assert(UnitTest_Marshaled());
}

using namespace Hydrogenium::StringPolicy::UnitTest;
