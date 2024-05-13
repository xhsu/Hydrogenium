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

	static constexpr bool UnitTest_New() noexcept	// requires C++20 constexpr new.
	{
		as_unmanaged_t functor{};
		auto const cpy1 = functor(ENG_TEXT_FWD);
		auto const cpy2 = functor(RMN_WTEXT_FWD);

		decltype(ENG_TEXT_FWD) const view1{ cpy1 };
		decltype(RMN_WTEXT_FWD) const view2{ cpy2 };

		bool const bResult = view1 == ENG_TEXT_FWD && view2 == cpy2;

		delete[] cpy1;
		delete[] cpy2;

		return bResult;
	}
	static_assert(UnitTest_New());
}

void UnitTest_Runtime()
{

}