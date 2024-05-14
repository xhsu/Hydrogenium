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
}
