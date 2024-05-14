﻿#include "Precompiled.hpp"
#include "UtlString.hpp"



using namespace Hydrogenium::StringPolicy;
using namespace Hydrogenium::UnitTest;

namespace Hydrogenium::String::UnitTest
{
	// Str series

	static_assert(StrI::Cmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrI::Cmp("abc", "DEF") < 0 && Str::Cmp("abc", "DEF") > 0);
	static_assert(StrI::Cmp("GHI", "def") > 0 && Str::Cmp("GHI", "def") < 0);
	static_assert(Str::Cmp(u8"你好", u8"你好") == 0 && Str::Cmp(u8"你好", u8"你好嗎") < 0);
	static_assert(StrNR::Cmp("dynamic_cast", "static_cast", 7) == 0 && StrNR::Cmp("dynamic_cast", "static_cast", 8) < 0);	// just like ends_with

	static_assert(Str::Chr("Try not", 't') == "t" && StrI::Chr("Try not", 'T') == "Try not");
	static_assert(StrN::Chr("Try not", 't', 4).empty() && StrNI::Chr("Try not", 't', 4) == "Try ");	// #NO_URGENT this is not good. the return of StrNI series should kept the original length.
	static_assert(StrR::Chr("Try not", 'T') == "Try not" && StrIR::Chr("Try not", 'T') == "t");
	static_assert(StrNR::Chr("Try not", 'T', 4).empty() && StrNIR::Chr("Try not", 'T', 4) == "t");

	// Wcs series

	static_assert(WcsI::Cmp(L"a0b1c2", L"A0B1C2") == 0);
	static_assert(WcsI::Cmp(L"abc", L"DEF") < 0 && Wcs::Cmp(L"abc", L"DEF") > 0);
	static_assert(WcsI::Cmp(L"GHI", L"def") > 0 && Wcs::Cmp(L"GHI", L"def") < 0);
	static_assert(Wcs::Cmp(L"你好", L"你好") == 0 && Wcs::Cmp(L"你好", L"你好嗎") < 0);
	static_assert(WcsN::Cmp(L"你好", L"你好嗎", 2) == 0 && WcsN::Cmp(L"你好", L"你好嗎", 3) < 0);

	static_assert(Wcs::Chr(L"Try not", L't') == L"t" && WcsI::Chr(L"Try not", L'T') == L"Try not");
	static_assert(WcsN::Chr(L"Try not", L't', 4).empty() && WcsNI::Chr(L"Try not", L't', 4) == L"Try ");

	// Mbs series

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

// Cmp
namespace Hydrogenium::String::UnitTest
{
	static_assert(StrI::Cmp(ENG_ALPHABET_LOWER_FWD, ENG_ALPHABET_UPPER_FWD) == 0);
#ifdef HYDROGENIUM_UTL_UNICODE
	static_assert(WcsI::Cmp(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_UPPER_FWD_W) == 0);
	static_assert(MbsI::Cmp(UKR_ALPHABET_LOWER_FWD_U8, UKR_ALPHABET_UPPER_FWD_U8) == 0);
#endif
}

// Dup, Rev
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Dup("a0b1c2") == StrI::Dup("a0b1c2"));
	static_assert(StrR::Dup(ASCII_NUMBERS_FWD) == ASCII_NUMBERS_BWD);
	static_assert(WcsR::Dup(RMN_NUMBERS_BWD_W) == RMN_NUMBERS_FWD_W);
	static_assert(Mbs::Cmp(MbsR::Dup(CJK_NUMBERS_FWD_U8), CJK_NUMBERS_BWD_U8) == 0);	// #MSVC_BUGGED_compile_time_utf8

	static_assert(StrN::Dup("a0b1c2", 3) == StrNI::Dup("a0b1c2", 3));
	static_assert(MbsN::Dup(CJK_NUMBERS_FWD_U8, 0).empty());
	static_assert(StrN::Cmp(StrNR::Dup(ASCII_NUMBERS_FWD, 5), ASCII_NUMBERS_BWD, 5) == 0);
	static_assert(WcsN::Cmp(WcsNR::Dup(RMN_NUMBERS_BWD_W, 5), RMN_NUMBERS_FWD_W, 5) == 0);
	static_assert(MbsN::Cmp(MbsN::Dup(CJK_NUMBERS_FWD_U8, 5), CJK_NUMBERS_FWD_U8, 5) == 0);
	static_assert(MbsN::Cmp(MbsNR::Dup(CJK_NUMBERS_FWD_U8, 5), CJK_NUMBERS_BWD_U8, 5) == 0);
}

// Lwr
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Lwr(ASCII_NUMBERS_BWD) == ASCII_NUMBERS_BWD);
	static_assert(Mbs::Cmp(MbsR::Lwr(CJK_NUMBERS_BWD_U8), CJK_NUMBERS_FWD_U8) == 0);	// effectively just producing a copy. #MSVC_BUGGED_compile_time_utf8
	static_assert(Str::Lwr(ENG_ALPHABET_UPPER_FWD) == ENG_ALPHABET_LOWER_FWD);

	static_assert(StrN::Cmp(StrNR::Lwr(ENG_ALPHABET_UPPER_FWD, 10), ENG_ALPHABET_LOWER_BWD, 10) == 0);

#ifdef HYDROGENIUM_UTL_UNICODE
	static_assert(Wcs::Lwr(ELL_ALPHABET_UPPER_FWD_W) == ELL_ALPHABET_LOWER_FWD_W);
	static_assert(Mbs::Cmp(Mbs::Lwr(UKR_ALPHABET_UPPER_FWD_U8), UKR_ALPHABET_LOWER_FWD_U8) == 0);	// #MSVC_BUGGED_compile_time_utf8

	static_assert(WcsN::Cmp(WcsNR::Lwr(ELL_ALPHABET_UPPER_FWD_W, 10), ELL_ALPHABET_LOWER_BWD_W, 10) == 0);
	static_assert(MbsN::Cmp(MbsNR::Lwr(UKR_ALPHABET_UPPER_FWD_U8, 10), UKR_ALPHABET_LOWER_BWD_U8, 10) == 0);
#endif
}

// PBrk, SpnP, CSpn, Spn
namespace Hydrogenium::String::UnitTest
{
	static_assert(Wcs::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡") == L"葡萄不吐葡萄皮");
	static_assert(WcsN::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡", 1).empty());
	static_assert(Mbs::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡") == u8"葡萄不吐葡萄皮");
	static_assert(MbsN::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡", 1).empty());
	static_assert(WcsR::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡") == L"葡萄皮");
	static_assert(WcsNR::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡", 2).empty());
	static_assert(MbsR::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡") == u8"葡萄皮");
	static_assert(MbsNR::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡", 2).empty());

	static_assert(StrI::PBrk("Try not", "tr") == "Try not");
	static_assert(StrIR::PBrk("Try not", "tr") == "t");
	static_assert(StrI::SpnP("Try not", "tr") == "y not");
	static_assert(StrIR::SpnP("Try not", "tr") == "ot");

	static_assert(MbsRCSpn(u8"吃葡萄不吐葡萄皮", u8"吐葡") == 5);
	//                        └────────┘  ← searching dir
	static_assert(MbsRSpn(u8"吃葡萄不吐葡萄皮", u8"葡萄皮") == 4);
	//                       └──────┘     ← searching dir
	static_assert(MbsCSpn("abcde312$#@", "*$#") == 8);
	//                     └───────┘
	static_assert(MbsSpn("abcde312$#@", "qwertyuiopasdfghjklzxcvbnm") == 5);
	//                    └────┘
}


extern void UnitTest_Runtime();

int main(int, char* []) noexcept
{
	using namespace Hydrogenium;
	using namespace Hydrogenium::UnitTest;
	using namespace Hydrogenium::String::UnitTest;

	//UnitTest_Runtime();
}
