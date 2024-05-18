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
}
