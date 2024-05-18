#include "Precompiled.hpp"
#include "UtlString.hpp"

namespace Hydrogenium::UnitTest
{
	static_assert(ReverseIterator<decltype(ASCII_NUMBERS_FWD.rbegin())>);
	static_assert(!ReverseIterator<decltype(ASCII_NUMBERS_FWD.begin())>);

	static_assert(*ASCII_NUMBERS_FWD.begin() == *ToReverseIter(ASCII_NUMBERS_FWD.begin()));
	static_assert(*ASCII_NUMBERS_FWD.rbegin() == *ToReverseIter(ASCII_NUMBERS_FWD.rbegin()));
	static_assert(*ASCII_NUMBERS_FWD.begin() == *ToForwardIter(ASCII_NUMBERS_FWD.begin()));
	static_assert(*ASCII_NUMBERS_FWD.rbegin() == *ToForwardIter(ASCII_NUMBERS_FWD.rbegin()));

	static_assert(ToForwardIter(std::make_reverse_iterator(ASCII_NUMBERS_FWD.rbegin())) == (ASCII_NUMBERS_FWD.end() - 1));
	static_assert(ToReverseIter(std::make_reverse_iterator(ASCII_NUMBERS_FWD.rbegin())) == ASCII_NUMBERS_FWD.rbegin());

	template <typename C>
	consteval bool UnitTest_StringIterConcept()
	{
		std::basic_string_view<C> sv{};
		std::basic_string<C> s{};

		static_assert(IteratorOf<decltype(sv.begin()), C>);
		static_assert(IteratorOf<decltype(sv.end()), C>);
		static_assert(IteratorOf<decltype(sv.rbegin()), C>);
		static_assert(IteratorOf<decltype(sv.rend()), C>);

		static_assert(IteratorOf<decltype(sv.cbegin()), C>);
		static_assert(IteratorOf<decltype(sv.cend()), C>);
		static_assert(IteratorOf<decltype(sv.crbegin()), C>);
		static_assert(IteratorOf<decltype(sv.crend()), C>);

		static_assert(IteratorOf<decltype(s.begin()), C>);
		static_assert(IteratorOf<decltype(s.end()), C>);
		static_assert(IteratorOf<decltype(s.rbegin()), C>);
		static_assert(IteratorOf<decltype(s.rend()), C>);

		static_assert(IteratorOf<decltype(s.cbegin()), C>);
		static_assert(IteratorOf<decltype(s.cend()), C>);
		static_assert(IteratorOf<decltype(s.crbegin()), C>);
		static_assert(IteratorOf<decltype(s.crend()), C>);

		static_assert(IteratorOf<C*, C>);
		static_assert(IteratorOf<C const*, C>);

		static_assert(!IteratorOf<uintptr_t, C>);
		static_assert(!IteratorOf<void*, C>);

		return true;
	}
	static_assert(UnitTest_StringIterConcept<char>());
	static_assert(UnitTest_StringIterConcept<wchar_t>());

	static constexpr bool UnitTest_UtfAt(auto&& seq, auto&& ans, int GRAPHEME_COUNT = 10) noexcept
	{
		for (int i = 0; i < GRAPHEME_COUNT; ++i)
			if (auto const ch = UtfAt(seq, i); ch != ans[i])
				return false;

		for (int i = -1; i > -(GRAPHEME_COUNT + 1); --i)
			if (auto const ch = UtfAt(seq, i); ch != ans[GRAPHEME_COUNT + i])
				return false;

		return true;
	}
	static_assert(UnitTest_UtfAt(ASCII_NUMBERS_FWD, ASCII_NUMBERS_FWD_U32ARR));
	static_assert(UnitTest_UtfAt(CJK_NUMBERS_FWD_U8, CJK_NUMBERS_FWD_U32ARR));
	static_assert(UnitTest_UtfAt(RMN_NUMBERS_FWD_W, RMN_NUMBERS_FWD_U32ARR));
}

using namespace Hydrogenium::UnitTest;
