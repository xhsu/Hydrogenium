#include "Precompiled.hpp"
#include "UtlString.hpp"

namespace Hydrogenium::String::UnitTest
{
	using namespace StringPolicy;

	using Str = Utils<>;
	using StrI = Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored>;
	using StrN = Utils<char, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n>;
	using StrNI = Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n>;
	using StrR = Utils<char, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_len, Direction::backwards{} > ;
	using StrIR = Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_len, Direction::backwards{} > ;
	using StrNR = Utils<char, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n, Direction::backwards{} > ;
	using StrNIR = Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n, Direction::backwards{} > ;

	static_assert(StrI::Cmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrI::Cmp("abc", "DEF") < 0 && Str::Cmp("abc", "DEF") > 0);
	static_assert(StrI::Cmp("GHI", "def") > 0 && Str::Cmp("GHI", "def") < 0);
	static_assert(Str::Cmp(u8"你好", u8"你好") == 0 && Str::Cmp(u8"你好", u8"你好嗎") < 0);
	static_assert(StrNR::Cmp("dynamic_cast", "static_cast", 7) == 0 && StrNR::Cmp("dynamic_cast", "static_cast", 8) < 0);	// just like ends_with

	static_assert(Str::Chr("Try not", 't') == "t" && StrI::Chr("Try not", 'T') == "Try not");
	static_assert(StrN::Chr("Try not", 't', 4).empty() && StrNI::Chr("Try not", 't', 4) == "Try ");	// #NO_URGENT this is not good. the return of StrNI series should kept the original length.
	static_assert(StrR::Chr("Try not", 'T') == "Try not" && StrIR::Chr("Try not", 'T') == "t");
	static_assert(StrNR::Chr("Try not", 'T', 4).empty() && StrNIR::Chr("Try not", 'T', 4) == "t");

	using Wcs = Utils<wchar_t>;
	using WcsI = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::case_ignored>;
	using WcsN = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n>;
	using WcsNI = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n>;

	static_assert(WcsI::Cmp(L"a0b1c2", L"A0B1C2") == 0);
	static_assert(WcsI::Cmp(L"abc", L"DEF") < 0 && Wcs::Cmp(L"abc", L"DEF") > 0);
	static_assert(WcsI::Cmp(L"GHI", L"def") > 0 && Wcs::Cmp(L"GHI", L"def") < 0);
	static_assert(Wcs::Cmp(L"你好", L"你好") == 0 && Wcs::Cmp(L"你好", L"你好嗎") < 0);
	static_assert(WcsN::Cmp(L"你好", L"你好嗎", 2) == 0 && WcsN::Cmp(L"你好", L"你好嗎", 3) < 0);

	static_assert(Wcs::Chr(L"Try not", L't') == L"t" && WcsI::Chr(L"Try not", L'T') == L"Try not");
	static_assert(WcsN::Chr(L"Try not", L't', 4).empty() && WcsNI::Chr(L"Try not", L't', 4) == L"Try ");

	using Mbs = Utils<char, Iterating::as_multibytes>;
	using MbsN = Utils<char, Iterating::as_multibytes, Comparing::regular, Counter::cap_at_n>;
	using MbsR = Utils<char, Iterating::as_multibytes, Comparing::regular, Counter::cap_at_len, Direction::backwards{} > ;
	using MbsNR = Utils<char, Iterating::as_multibytes, Comparing::regular, Counter::cap_at_n, Direction::backwards{} > ;

	static_assert(Mbs::Cnt(u8"Heraclius") == Str::Cnt(u8"Heraclius"));
	static_assert(Mbs::Cnt(u8"Ἡράκλειος") == 9);
	static_assert(Mbs::Cnt(u8"Héraclius") == 9);
	static_assert(Mbs::Cnt(u8"Ираклий") == 7);
	static_assert(Mbs::Cnt(u8"ヘラクレイオス") == 7);
	static_assert(Mbs::Cnt(u8"希拉克略") == 4);
	static_assert(MbsN::Cnt(u8"Heráclio", 5) == 5);
	static_assert(MbsN::Cnt(u8"Іраклій", 0x100) == 7);

	static_assert(Mbs::Chr(u8"你好", '\xE5').empty() && Str::Chr(u8"你好", '\xE5') == u8"好");	// u8"好" == 0xE5 0xA5 0xBD

	static_assert(MbsN::Cmp(u8"你好", u8"你好嗎", 2) == 0 && MbsN::Cmp(u8"你好", u8"你好嗎", 3) < 0);
	static_assert(MbsN::Cmp(u8"吃葡萄不吐葡萄皮", "不吃葡萄倒吐葡萄皮", 4) > 0 && MbsNR::Cmp(u8"吃葡萄不吐葡萄皮", "不吃葡萄倒吐葡萄皮", 4) == 0);	// U'吃' == \x5403, U'不' == \x4E0D
}

int main(int, char* []) noexcept
{
	using namespace Hydrogenium;
	using namespace Hydrogenium::String::UnitTest;

	static_assert(CType<char>::IsAlNum('a') && !CType<char>::IsAlNum('!'));
	static_assert(CType<unsigned char>::IsAlNum('a') && !CType<unsigned char>::IsAlNum('!'));
	static_assert(CType<signed char>::IsAlNum('a') && !CType<signed char>::IsAlNum('!'));
	static_assert(CType<char16_t>::IsAlNum('a') && !CType<char16_t>::IsAlNum('!'));
	static_assert(CType<wchar_t>::IsAlNum('a') && !CType<wchar_t>::IsAlNum('!'));
	static_assert(CType<char32_t>::IsAlNum('a') && !CType<char32_t>::IsAlNum('!'));

	fmt::println("{}", StrI::Cmp(u8"你好", u8"你好"));
	fmt::println("{}", StrI::Cmp(u8"你好", u8"你好嗎"));
	fmt::println("{}", Mbs::Cnt(u8"هرقل"));
	fmt::println(u8"<empty>: {}, 你好: {}; 你好嗎: {}", Mbs::Cnt(""), Mbs::Cnt(u8"你好"), Mbs::Cnt(u8"你好嗎"));
}
