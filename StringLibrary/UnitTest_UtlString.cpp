#include "Precompiled.hpp"
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
	static_assert(MbsI::Cmp(DEU_ALPHABET_LOWER_FWD_U8, DEU_ALPHABET_UPPER_FWD_U8) == 0);
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

	// Internal DupV()

	static_assert(StrNR::detail::DupV(ASCII_NUMBERS_FWD, 5) == "56789");	// purpose: take last five graphemes.
	static_assert(WcsN::detail::DupV(RMN_NUMBERS_FWD_W, 5) == L"ⅠⅡⅢⅣⅤ");	// purpose: take first five graphemes.
	static_assert(MbsN::detail::DupV(CJK_NUMBERS_FWD_U8, 5) == u8"零一二三四");	// purpose: verify multibyte case.
	static_assert(MbsNR::detail::DupV(CJK_NUMBERS_FWD_U8, 20) == CJK_NUMBERS_FWD_U8);	// purpose: verify the meaning of 'count' parameter here, as grapheme count, not byte count.
}

// Fry
namespace Hydrogenium::String::UnitTest
{
	void UnitTest_Fry() noexcept
	{
		std::string test1(0x10, ' ');
		std::wstring test2(0x10, L' ');

		Str::detail::Fry(&test1);
		Wcs::detail::Fry(&test2);
	}
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
	static_assert(Mbs::Cmp(Mbs::Lwr(DEU_ALPHABET_UPPER_FWD_U8), DEU_ALPHABET_LOWER_FWD_U8) == 0);	// #MSVC_BUGGED_compile_time_utf8

	static_assert(WcsN::Cmp(WcsNR::Lwr(ELL_ALPHABET_UPPER_FWD_W, 10), ELL_ALPHABET_LOWER_BWD_W, 10) == 0);
	static_assert(MbsN::Cmp(MbsNR::Lwr(UKR_ALPHABET_UPPER_FWD_U8, 10), UKR_ALPHABET_LOWER_BWD_U8, 10) == 0);
	static_assert(MbsN::Cmp(MbsNR::Lwr(DEU_ALPHABET_UPPER_FWD_U8, 10), DEU_ALPHABET_LOWER_BWD_U8, 10) == 0);
#endif

	constexpr bool UnitTest_StrLwr() noexcept
	{
		std::string str{ DEU_ALPHABET_UPPER_FWD_U8 };
		MbsR::Lwr(&str);

		return Mbs::Cmp(str, DEU_ALPHABET_LOWER_BWD_U8) == 0;
	}
	static_assert(UnitTest_StrLwr());	// purpose: testing in_place usage.
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
	static_assert(MbsR::detail::CSpnR(u8"吃葡萄不吐葡萄皮", u8"吐葡") == 2);
	//                                            └───┘  ← searching & indexing dir.
	static_assert(MbsR::detail::CSpnR(u8"aäbcdefghijklmnoöpqrsßtuüvwxyz", u8"äöüß") == 5);
	//                                                           └────┘  ← searching & indexing dir.
	static_assert(Mbs::detail::CSpnR(u8"aäbcdefghijklmnoöpqrsßtuüvwxyz", u8"äöüß") == 1);
	//                                  └┘                               ← searching & indexing dir.
}

// Str
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Str("", "").empty());
	static_assert(Str::Str("", "abc").empty());
	static_assert(StrN::Str(ASCII_NUMBERS_FWD, "345", 6) == "345");
	static_assert(Str::Str(ASCII_NUMBERS_FWD, "345") == ASCII_NUMBERS_FWD.substr(3));
	static_assert(Str::Str(ASCII_NUMBERS_FWD, "90").empty());	// purpose: this is different from strchr()!

	static_assert(Wcs::Str(L"吃葡萄不吐葡萄皮", L"葡萄") == L"葡萄不吐葡萄皮");
	static_assert(WcsR::Str(L"吃葡萄不吐葡萄皮", L"葡萄") == L"葡萄皮");
	static_assert(Mbs::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄") == u8"葡萄不吐葡萄皮");
	static_assert(MbsR::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄") == u8"葡萄皮");
	static_assert(MbsN::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄", 3) == u8"葡萄");	// purpose: verify the multibytes can be correctly parsed as groups.
	static_assert(MbsNR::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄", 3) == u8"葡萄皮");

	static_assert(WcsR::Str(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_LOWER_BWD_W).empty());	// purpose: verify the reverse mode has nothing to do with substr dir.
	static_assert(WcsI::Str(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_UPPER_FWD_W.substr(10)) == ELL_ALPHABET_LOWER_FWD_W.substr(10));
	static_assert(WcsNI::Str(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_UPPER_FWD_W.substr(10), 10).empty());

	static_assert(MbsR::Str(DEU_ALPHABET_LOWER_FWD_U8, DEU_ALPHABET_LOWER_BWD_U8).empty());	// purpose: verify the reverse mode has nothing to do with substr dir.
	static_assert(MbsI::Str(DEU_ALPHABET_LOWER_FWD_U8, MbsR::detail::DupV(DEU_ALPHABET_UPPER_FWD_U8, 10)) == MbsR::detail::DupV(DEU_ALPHABET_LOWER_FWD_U8, 10));
	static_assert(MbsNI::Str(DEU_ALPHABET_LOWER_FWD_U8, MbsR::detail::DupV(DEU_ALPHABET_UPPER_FWD_U8, 10), 10).empty());
}

// Tok
namespace Hydrogenium::String::UnitTest
{
	template <typename T>
	bool UnitTest_StrTok(std::ranges::input_range auto&& view, auto&& delim, std::ranges::range auto&& ans) noexcept
	{
		bool ret = true;
		auto const loop_count = ans.size() + 1;	// additional one to check the respounce.
		size_t i = 0;

		for (auto sv = T::Tok(view, delim); i < loop_count; sv = T::Tok(std::nullopt, delim), ++i)
		{
			if (i < ans.size())
			{
				if (sv == ans[i])
					continue;

				if constexpr (typeid(view[0]) == typeid(char))
					fmt::print("{:?} != {:?}, at loop {} of {}\n", sv, ans[i], i, loop_count);
				else
					fmt::print(L"{:?} != {:?}, at loop {} of {}\n", sv, ans[i], i, loop_count);

				ret = false;
			}
			else
			{
				if (sv.empty())
					continue;

				if constexpr (typeid(view[0]) == typeid(char))
					fmt::print("{:?} != null, at loop {} of {}\n", sv, i, loop_count);
				else
					fmt::print(L"{:?} != null, at loop {} of {}\n", sv, i, loop_count);

				ret = false;
			}
		}

		return ret;
	}

	void UnitTest_StrTok() noexcept
	{
		assert(UnitTest_StrTok<Str>("hello, world", " ,\t", std::vector{ "hello", "world" }));
		assert(UnitTest_StrTok<StrR>("hello, world", " ,\t", std::vector{ "hello", "world" } | std::views::reverse));
		assert(UnitTest_StrTok<Str>("", " ,\t", std::views::empty<std::string_view>));
		assert(UnitTest_StrTok<StrR>("", "", std::views::empty<std::string_view>));
		assert(UnitTest_StrTok<WcsI>(ELL_ALPHABET_UPPER_FWD_W, L"αειουω", std::vector{ L"ΒΓΔ", L"ΖΗΘ", L"ΚΛΜΝΞ", L"ΠΡ΢ΣΤ", L"ΦΧΨ" }));
		assert(UnitTest_StrTok<WcsIR>(ELL_ALPHABET_UPPER_FWD_W, L"αειουω", std::vector{ L"ΒΓΔ", L"ΖΗΘ", L"ΚΛΜΝΞ", L"ΠΡ΢ΣΤ", L"ΦΧΨ" } | std::views::reverse));
		assert(UnitTest_StrTok<MbsI>(DEU_ALPHABET_UPPER_FWD_U8, "aeiou", std::vector{ u8"ÄBCD", u8"FGH", u8"JKLMN", u8"ÖPQRSẞT", u8"ÜVWXYZ" }));
		assert(UnitTest_StrTok<MbsIR>(DEU_ALPHABET_UPPER_FWD_U8, "aeiou", std::vector{ u8"ÄBCD", u8"FGH", u8"JKLMN", u8"ÖPQRSẞT", u8"ÜVWXYZ" } | std::views::reverse));
	}
}



extern void UnitTest_Runtime();

int main(int, char* []) noexcept
{
	using namespace std;
	using namespace Hydrogenium;
	using namespace Hydrogenium::String;
	using namespace Hydrogenium::UnitTest;
	using namespace Hydrogenium::String::UnitTest;

	UnitTest_StrTok();	// Run-time only.

	//UnitTest_Runtime();
}
