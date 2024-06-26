#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium::StringPolicy::Iterating;

namespace Hydrogenium::StringPolicy::UnitTest
{
	template <typename Policy>
	constexpr bool UnitTest_iterating_policy(std::ranges::input_range auto&& view, auto RangePolicy, std::span<char32_t const> results) noexcept
	{
		auto [bgn, it, ed] = Policy::Get(view, RangePolicy, 100);

		if (Policy::ValueOf(it) != results.front())
			return false;

		Policy::Arithmetic(it, bgn, ed, 3);
		if (Policy::ValueOf(it) != results[3])
			return false;

		Policy::Arithmetic(it, bgn, ed, 0);
		if (Policy::ValueOf(it) != results[3])
			return false;

		Policy::Arithmetic(it, bgn, ed, 100);
		if (it != ed)
			return false;

		Policy::Arithmetic(it, bgn, ed, -4);
		if (Policy::ValueOf(it) != results[6])	// 'end' is the 10th element.
			return false;

		Policy::Arithmetic(it, bgn, ed, -100);
		if (Policy::ValueOf(it) != results[0])	// NOT it == bgn!! in the case of reverse UTF sequence, the iterator will be at bgn if and only if the char is ASCII.
			return false;

		return true;
	}

	static_assert(UnitTest_iterating_policy<as_normal_ptr_t>(ASCII_NUMBERS_FWD, Direction::front_to_back, ASCII_NUMBERS_FWD_U32ARR));
	static_assert(UnitTest_iterating_policy<as_normal_ptr_t>(ASCII_NUMBERS_FWD, Direction::back_to_front, ASCII_NUMBERS_BWD_U32ARR));

	static_assert(UnitTest_iterating_policy<as_multibytes_t>(CJK_NUMBERS_FWD_U8, Direction::front_to_back, CJK_NUMBERS_FWD_U32ARR));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(CJK_NUMBERS_FWD_U8, Direction::back_to_front, CJK_NUMBERS_BWD_U32ARR));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(RMN_NUMBERS_FWD_W, Direction::front_to_back, RMN_NUMBERS_FWD_U32ARR));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(RMN_NUMBERS_FWD_W, Direction::back_to_front, RMN_NUMBERS_BWD_U32ARR));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(ASCII_NUMBERS_FWD, Direction::front_to_back, ASCII_NUMBERS_FWD_U32ARR));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(ASCII_NUMBERS_FWD, Direction::back_to_front, ASCII_NUMBERS_BWD_U32ARR));
}
