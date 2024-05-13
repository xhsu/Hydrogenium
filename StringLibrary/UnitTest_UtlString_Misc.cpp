#include "Precompiled.hpp"
#include "UtlString.hpp"

namespace Hydrogenium::UnitTest
{
	static_assert(ReverseIterator<decltype(ENG_TEXT_FWD.rbegin())>);
	static_assert(!ReverseIterator<decltype(ENG_TEXT_FWD.begin())>);

	static_assert(*ENG_TEXT_FWD.begin() == *ToReverseIter(ENG_TEXT_FWD.begin()));
	static_assert(*ENG_TEXT_FWD.rbegin() == *ToReverseIter(ENG_TEXT_FWD.rbegin()));
	static_assert(*ENG_TEXT_FWD.begin() == *ToForwardIter(ENG_TEXT_FWD.begin()));
	static_assert(*ENG_TEXT_FWD.rbegin() == *ToForwardIter(ENG_TEXT_FWD.rbegin()));

	static_assert(ToForwardIter(std::make_reverse_iterator(ENG_TEXT_FWD.rbegin())) == (ENG_TEXT_FWD.end() - 1));
	static_assert(ToReverseIter(std::make_reverse_iterator(ENG_TEXT_FWD.rbegin())) == ENG_TEXT_FWD.rbegin());
}
