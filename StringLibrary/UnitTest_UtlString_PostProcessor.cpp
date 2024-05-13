#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium::StringPolicy;
using namespace Hydrogenium::StringPolicy::Result;

using std::string_view;

namespace Hydrogenium::StringPolicy::UnitTest
{
	static_assert(std::is_same_v<std::invoke_result_t<as_pointer_t, string_view&, string_view&&, std::nullptr_t>, const char*>);
	static_assert(as_pointer_t{}(ENG_TEXT_FWD, ENG_TEXT_FWD, nullptr) == &ENG_TEXT_FWD[0]);
	static_assert(as_pointer_t{}(ENG_TEXT_FWD, ENG_TEXT_FWD.substr(9), nullptr) == &ENG_TEXT_FWD[9]);
	static_assert(as_pointer_t{}(ENG_TEXT_FWD, ENG_TEXT_FWD.substr(10), nullptr) == nullptr);
	static_assert(as_pointer_t{}(nullptr, string_view{ "" }, nullptr) == nullptr);

	static_assert(std::is_same_v<std::invoke_result_t<as_position_t, string_view&, string_view&&, std::nullptr_t>, std::ptrdiff_t>);
	static_assert(as_position_t{}(ENG_TEXT_FWD, ENG_TEXT_FWD.substr(5), Iterating::as_regular_ptr) == 5 && ENG_TEXT_FWD.substr(5)[0] == ENG_TEXT_FWD[5]);
	static_assert(as_position_t{}(ENG_TEXT_FWD, ENG_TEXT_FWD.substr(5), Iterating::as_regular_ptr) == 5);
	static_assert(as_position_t{}(ENG_TEXT_FWD, string_view{ "" }, Iterating::as_regular_ptr) == -1);	// error reserved value
	static_assert(as_position_t{}(CHN_TEXT_FWD, CHN_TEXT_FWD.substr(9 * 3), Iterating::as_regular_ptr) == (9 * 3));
	static_assert(as_position_t{}(CHN_TEXT_FWD, CHN_TEXT_FWD.substr(9 * 3), Iterating::as_multibytes) == 9);
	static_assert(as_position_t{}(CHN_TEXT_FWD, CHN_TEXT_FWD.substr(10 * 3), Iterating::as_multibytes) == -1);	// error reserved value

	static_assert(std::is_same_v<std::invoke_result_t<as_view_t, string_view&, string_view&&, std::nullptr_t>, string_view>);
	static_assert(as_view_t{}(ENG_TEXT_FWD, ENG_TEXT_FWD.substr(9), nullptr) == ENG_TEXT_FWD.substr(9));

	static constexpr bool UnitTest_Unmanaged() noexcept	// requires C++20 constexpr new.
	{
		auto [cn_txt_abs_begin, cn_txt_it, cn_txt_end]
			= Iterating::as_multibytes.Get(CHN_TEXT_FWD, Direction::back_to_front, 100);

		as_unmanaged_t unmanaged{};

		auto const cpy1 = unmanaged(ENG_TEXT_FWD.begin(), ENG_TEXT_FWD.end(), Iterating::as_regular_ptr);
		auto const cpy2 = unmanaged(RMN_WTEXT_FWD.rbegin(), RMN_WTEXT_FWD.rend(), Iterating::as_regular_ptr);
		auto const cpy3 = unmanaged(cn_txt_it, cn_txt_end, Iterating::as_multibytes);

		decltype(ENG_TEXT_FWD) const view1{ cpy1 };
		decltype(RMN_WTEXT_FWD) const view2{ cpy2 };
		decltype(CHN_TEXT_FWD) const view3{ cpy3 };

		bool const bResult =
			view1 == ENG_TEXT_FWD
			&& view2 == RMN_WTEXT_BWD
			&& Mbs::Cmp(view3, CHN_TEXT_BWD) == 0	// #MSVC_BUGGED_compile_time_utf8
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
			= Iterating::as_multibytes.Get(CHN_TEXT_FWD, Direction::back_to_front, 5);

		as_marshaled_t marshall{};

		auto cpy1 = marshall(ENG_TEXT_FWD.begin(), ENG_TEXT_FWD.begin() + 5, Iterating::as_regular_ptr);
		auto cpy2 = marshall(RMN_WTEXT_FWD.rbegin(), RMN_WTEXT_FWD.rbegin() + 5, Iterating::as_regular_ptr);
		auto cpy3 = marshall(cn_txt_it, cn_txt_end, Iterating::as_multibytes);

		bool const bResult =
			ENG_TEXT_FWD.starts_with(cpy1)
			&& RMN_WTEXT_BWD.starts_with(cpy2)
			&& MbsN::Cmp(cpy3, CHN_TEXT_BWD, 5) == 0
			;

		return bResult;
	}
	static_assert(UnitTest_Marshaled());
}

using namespace Hydrogenium::StringPolicy::UnitTest;

void UnitTest_Runtime()
{
	assert(UnitTest_Unmanaged());
	assert(UnitTest_Marshaled());
}