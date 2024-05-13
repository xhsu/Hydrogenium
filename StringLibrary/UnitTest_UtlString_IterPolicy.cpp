#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium::StringPolicy::Iterating;

namespace Hydrogenium::StringPolicy::UnitTest
{
	template <typename Policy>
	constexpr bool UnitTest_iterating_policy(auto&& bgn, auto&& ed, std::span<char32_t const> results) noexcept
	{
		auto it = bgn;
		Policy::Initialize(it, bgn, ed);
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

	static_assert(UnitTest_iterating_policy<as_normal_ptr_t>(ENG_TEXT_FWD.begin(), ENG_TEXT_FWD.end(), ENG_WORDS_FWD));
	static_assert(UnitTest_iterating_policy<as_normal_ptr_t>(ENG_TEXT_FWD.rbegin(), ENG_TEXT_FWD.rend(), ENG_WORDS_BWD));

	static_assert(UnitTest_iterating_policy<as_multibytes_t>(CHN_TEXT_FWD.begin(), CHN_TEXT_FWD.end(), CHN_WORDS_FWD));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(CHN_TEXT_FWD.rbegin(), CHN_TEXT_FWD.rend(), CHN_WORDS_BWD));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(RMN_WTEXT_FWD.begin(), RMN_WTEXT_FWD.end(), RMN_WORDS_FWD));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(RMN_WTEXT_FWD.rbegin(), RMN_WTEXT_FWD.rend(), RMN_WORDS_BWD));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(ENG_TEXT_FWD.begin(), ENG_TEXT_FWD.end(), ENG_WORDS_FWD));
	static_assert(UnitTest_iterating_policy<as_multibytes_t>(ENG_TEXT_FWD.rbegin(), ENG_TEXT_FWD.rend(), ENG_WORDS_BWD));
}
