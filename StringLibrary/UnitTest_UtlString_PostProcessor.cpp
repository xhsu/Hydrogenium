#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium::StringPolicy;
using namespace Hydrogenium::StringPolicy::Result;

using std::string_view;

namespace Hydrogenium::StringPolicy::UnitTest
{
	static_assert(std::is_same_v<std::invoke_result_t<as_pointer_t, string_view&, string_view&&, std::nullptr_t>, const char*>);
	static_assert(as_pointer_t{}(ASCII_NUMBERS_FWD, ASCII_NUMBERS_FWD, nullptr) == &ASCII_NUMBERS_FWD[0]);
	static_assert(as_pointer_t{}(ASCII_NUMBERS_FWD, ASCII_NUMBERS_FWD.substr(9), nullptr) == &ASCII_NUMBERS_FWD[9]);
	static_assert(as_pointer_t{}(ASCII_NUMBERS_FWD, ASCII_NUMBERS_FWD.substr(10), nullptr) == nullptr);
	static_assert(as_pointer_t{}(nullptr, string_view{ "" }, nullptr) == nullptr);

	static_assert(std::is_same_v<std::invoke_result_t<as_position_t, string_view&, string_view&&, std::nullptr_t>, std::ptrdiff_t>);
	static_assert(as_position_t{}(ASCII_NUMBERS_FWD, ASCII_NUMBERS_FWD.substr(5), Iterating::as_regular_ptr) == 5 && ASCII_NUMBERS_FWD.substr(5)[0] == ASCII_NUMBERS_FWD[5]);
	static_assert(as_position_t{}(ASCII_NUMBERS_FWD, ASCII_NUMBERS_FWD.substr(5), Iterating::as_regular_ptr) == 5);
	static_assert(as_position_t{}(ASCII_NUMBERS_FWD, string_view{ "" }, Iterating::as_regular_ptr) == 10);	// no found: return strcnt() equivlent.
	static_assert(as_position_t{}(CJK_NUMBERS_FWD_U8, CJK_NUMBERS_FWD_U8.substr(9 * 3), Iterating::as_regular_ptr) == (9 * 3));
	static_assert(as_position_t{}(CJK_NUMBERS_FWD_U8, CJK_NUMBERS_FWD_U8.substr(9 * 3), Iterating::as_multibytes) == 9);
	static_assert(as_position_t{}(CJK_NUMBERS_FWD_U8, CJK_NUMBERS_FWD_U8.substr(10 * 3), Iterating::as_multibytes) == 10);	// no found: return strcnt() equivlent.

	static_assert(std::is_same_v<std::invoke_result_t<as_view_t, string_view&, string_view&&, std::nullptr_t>, string_view>);
	static_assert(as_view_t{}(ASCII_NUMBERS_FWD, ASCII_NUMBERS_FWD.substr(9), nullptr) == ASCII_NUMBERS_FWD.substr(9));

	static constexpr bool UnitTest_Unmanaged() noexcept	// requires C++20 constexpr new.
	{
		auto [cn_txt_abs_begin, cn_txt_it, cn_txt_end]
			= Iterating::as_multibytes.Get(CJK_NUMBERS_FWD_U8, Direction::back_to_front, 100);

		as_unmanaged_t unmanaged{};

		auto const cpy1 = unmanaged(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.end(), Iterating::as_regular_ptr);
		auto const cpy2 = unmanaged(RMN_NUMBERS_FWD_W.rbegin(), RMN_NUMBERS_FWD_W.rend(), Iterating::as_regular_ptr);
		auto const cpy3 = unmanaged(cn_txt_it, cn_txt_end, Iterating::as_multibytes);

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

		as_marshaled_t marshall{};

		auto cpy1 = marshall(ASCII_NUMBERS_FWD.begin(), ASCII_NUMBERS_FWD.begin() + 5, Iterating::as_regular_ptr);
		auto cpy2 = marshall(RMN_NUMBERS_FWD_W.rbegin(), RMN_NUMBERS_FWD_W.rbegin() + 5, Iterating::as_regular_ptr);
		auto cpy3 = marshall(cn_txt_it, cn_txt_end, Iterating::as_multibytes);

		bool const bResult =
			ASCII_NUMBERS_FWD.starts_with(cpy1)
			&& RMN_NUMBERS_BWD_W.starts_with(cpy2)
			&& MbsN::Cmp(cpy3, CJK_NUMBERS_BWD_U8, 5) == 0
			;

		return bResult;
	}
	static_assert(UnitTest_Marshaled());
}

using namespace Hydrogenium::StringPolicy::UnitTest;
