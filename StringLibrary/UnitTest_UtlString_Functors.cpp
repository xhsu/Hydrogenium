﻿#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::String;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;



// Chr
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Chr("Try not", 't') == "t" && StrI::Chr("Try not", 'T') == "Try not");
	static_assert(Str::Chr("Try not", 't', 4).empty() && StrI::Chr("Try not", 't', 4) == "Try ");	// #NO_URGENT this is not good. The 'N' version will cap the returning view onto param 'count'. Purpose: test 'N' version.
	static_assert(StrR::Chr("Try not", 'T') == "Try not" && StrIR::Chr("Try not", 'T') == "t");
	static_assert(StrR::Chr("Try not", 'T', 4).empty() && StrIR::Chr("Try not", 'T', 4) == "t"); // Purpose: test 'N' version.

	static_assert(Wcs::Chr(L"Try not", L't') == L"t" && WcsI::Chr(L"Try not", L'T') == L"Try not");
	static_assert(Wcs::Chr(L"Try not", L't', 4).empty() && WcsI::Chr(L"Try not", L't', 4) == L"Try ");	// Purpose: test 'N' version. The 'N' version will not cap the returning view onto param 'count'.

	static_assert(Mbs::Chr(u8"你好", '\xE5').empty() && Str::Chr(u8"你好", '\xE5') == u8"好");	// u8"好" == 0xE5 0xA5 0xBD
}

// Cmp
namespace Hydrogenium::String::UnitTest
{
	// Str series

	static_assert(StrI::Cmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrI::Cmp("abc", "DEF") < 0 && Str::Cmp("abc", "DEF") > 0);
	static_assert(StrI::Cmp("GHI", "def") > 0 && Str::Cmp("GHI", "def") < 0);
	static_assert(Str::Cmp(u8"你好", u8"你好") == 0 && Str::Cmp(u8"你好", u8"你好嗎") < 0);
	static_assert(StrR::Cmp("dynamic_cast", "static_cast", 7) == 0 && StrR::Cmp("dynamic_cast", "static_cast", 8) < 0);	// just like ends_with. Purpose: test 'N' version.

	// Wcs series

	static_assert(WcsI::Cmp(L"a0b1c2", L"A0B1C2") == 0);
	static_assert(WcsI::Cmp(L"abc", L"DEF") < 0 && Wcs::Cmp(L"abc", L"DEF") > 0);
	static_assert(WcsI::Cmp(L"GHI", L"def") > 0 && Wcs::Cmp(L"GHI", L"def") < 0);
	static_assert(Wcs::Cmp(L"你好", L"你好") == 0 && Wcs::Cmp(L"你好", L"你好嗎") < 0);
	static_assert(Wcs::Cmp(L"你好", L"你好嗎", 2) == 0 && Wcs::Cmp(L"你好", L"你好嗎", 3) < 0);	// Purpose: test 'N' version.

	// Mbs series

	static_assert(Mbs::Cmp(u8"你好", u8"你好嗎", 2) == 0 && Mbs::Cmp(u8"你好", u8"你好嗎", 3) < 0);	// Purpose: test 'N' version.
	static_assert(Mbs::Cmp(u8"吃葡萄不吐葡萄皮", "不吃葡萄倒吐葡萄皮", 4) > 0 && MbsR::Cmp(u8"吃葡萄不吐葡萄皮", "不吃葡萄倒吐葡萄皮", 4) == 0);	// U'吃' == \x5403, U'不' == \x4E0D. Purpose: test 'N' version.

#ifdef HYDROGENIUM_UTL_UNICODE
	static_assert(StrI::Cmp(ENG_ALPHABET_LOWER_FWD, ENG_ALPHABET_UPPER_FWD) == 0);
	static_assert(WcsI::Cmp(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_UPPER_FWD_W) == 0);
	static_assert(MbsI::Cmp(UKR_ALPHABET_LOWER_FWD_U8, UKR_ALPHABET_UPPER_FWD_U8) == 0);
	static_assert(MbsI::Cmp(DEU_ALPHABET_LOWER_FWD_U8, DEU_ALPHABET_UPPER_FWD_U8) == 0);
#endif
}

// Cnt, Len
namespace Hydrogenium::String::UnitTest
{
	static_assert(Mbs::Cnt(u8"Heraclius") == Str::Cnt(u8"Heraclius"));
	static_assert(Mbs::Cnt(u8"Ἡράκλειος") == 9);
	static_assert(Mbs::Cnt(u8"Héraclius") == 9);
	static_assert(Mbs::Cnt(u8"Ираклий") == 7);
	static_assert(Mbs::Cnt(u8"ヘラクレイオス") == 7);
	static_assert(Mbs::Cnt(u8"希拉克略") == 4);
	static_assert(Mbs::Cnt(u8"Heráclio", 5) == 5);	// Purpose: test 'N' version.
	static_assert(Mbs::Cnt(u8"Іраклій", 0x100) == 7);	// Purpose: test 'N' version.

	static_assert(Str::Cnt(ASCII_NUMBERS_FWD) == StrR::Cnt(ASCII_NUMBERS_BWD));
	static_assert(Wcs::Cnt(RMN_NUMBERS_FWD_W) == WcsR::Cnt(RMN_NUMBERS_BWD_W));
	static_assert(Mbs::Cnt(CJK_NUMBERS_FWD_U8) == MbsR::Cnt(CJK_NUMBERS_BWD_U8));
	static_assert(Mbs::Cnt(DEU_ALPHABET_UPPER_FWD_U8) == MbsR::Cnt(DEU_ALPHABET_LOWER_FWD_U8));
	static_assert(Mbs::Cnt(DEU_ALPHABET_UPPER_BWD_U8, 10) == MbsR::Cnt(DEU_ALPHABET_LOWER_BWD_U8, 10));	// Purpose: test 'N' version.

	static_assert(StrLen(ASCII_NUMBERS_FWD) == ASCII_NUMBERS_FWD.length());
	static_assert(StrLen(CJK_NUMBERS_FWD_U8) == CJK_NUMBERS_FWD_U8.length());
}

// Dup, Rev, Sub
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Dup("a0b1c2") == StrI::Dup("a0b1c2"));
	static_assert(StrR::Dup(ASCII_NUMBERS_FWD) == ASCII_NUMBERS_BWD);
	static_assert(WcsR::Dup(RMN_NUMBERS_BWD_W) == RMN_NUMBERS_FWD_W);
	static_assert(Mbs::Cmp(MbsR::Dup(CJK_NUMBERS_FWD_U8), CJK_NUMBERS_BWD_U8) == 0);	// #MSVC_BUGGED_compile_time_utf8

	static_assert(Str::Dup("a0b1c2", 3) == StrI::Dup("a0b1c2", 3));	// Purpose: test 'N' version.
	static_assert(Mbs::Dup(CJK_NUMBERS_FWD_U8, 0).empty());	// Purpose: test 'N' version.
	static_assert(Str::Cmp(StrR::Dup(ASCII_NUMBERS_FWD, 5), ASCII_NUMBERS_BWD, 5) == 0);	// Purpose: test 'N' version.
	static_assert(Wcs::Cmp(WcsR::Dup(RMN_NUMBERS_BWD_W, 5), RMN_NUMBERS_FWD_W, 5) == 0);	// Purpose: test 'N' version.
	static_assert(Mbs::Cmp(Mbs::Dup(CJK_NUMBERS_FWD_U8, 5), CJK_NUMBERS_FWD_U8, 5) == 0);	// Purpose: test 'N' version.
	static_assert(Mbs::Cmp(MbsR::Dup(CJK_NUMBERS_FWD_U8, 5), CJK_NUMBERS_BWD_U8, 5) == 0);	// Purpose: test 'N' version.

	// Internal DupV(), mostly for 'N' version. (Formalized as Sub()).

	static_assert(StrR::Sub(ASCII_NUMBERS_FWD, 5) == "56789");	// purpose: take last five graphemes.
	static_assert(Wcs::Sub(RMN_NUMBERS_FWD_W, 5) == L"ⅠⅡⅢⅣⅤ");	// purpose: take first five graphemes.
	static_assert(Mbs::Sub(CJK_NUMBERS_FWD_U8, 5) == u8"零一二三四");	// purpose: verify multibyte case.
	static_assert(MbsR::Sub(CJK_NUMBERS_FWD_U8, 20) == CJK_NUMBERS_FWD_U8);	// purpose: verify the meaning of 'count' parameter here, as grapheme count, not byte count.
}

// Fry
namespace Hydrogenium::String::UnitTest
{
	void UnitTest_StrFry() noexcept
	{
		std::string test1(0x20, '\0');
		std::wstring test2(0x10, L'\0');

		Str::Fry(&test1, 0x10);
		Wcs::Fry(&test2);

		assert(test1.size() == 0x20);
		assert(test2.size() == 0x10);

		for (auto&& c : test1 | std::views::take(0x10))
			if (c == '\0')
				assert(false);

		for (auto&& c : test1 | std::views::drop(0x10))
			if (c != '\0')
				assert(false);

		for (auto&& c : test2)
			if (c == L'\0')
				assert(false);
	}
}

// Lwr
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Lwr(ASCII_NUMBERS_BWD) == ASCII_NUMBERS_BWD);
	static_assert(Mbs::Cmp(MbsR::Lwr(CJK_NUMBERS_BWD_U8), CJK_NUMBERS_FWD_U8) == 0);	// effectively just producing a copy. #MSVC_BUGGED_compile_time_utf8
	static_assert(Str::Lwr(ENG_ALPHABET_UPPER_FWD) == ENG_ALPHABET_LOWER_FWD);

	static_assert(Str::Cmp(StrR::Lwr(ENG_ALPHABET_UPPER_FWD, 10), ENG_ALPHABET_LOWER_BWD, 10) == 0);	// Purpose: test 'N' version.

#ifdef HYDROGENIUM_UTL_UNICODE
	static_assert(Wcs::Lwr(ELL_ALPHABET_UPPER_FWD_W) == ELL_ALPHABET_LOWER_FWD_W);
	static_assert(Mbs::Cmp(Mbs::Lwr(UKR_ALPHABET_UPPER_FWD_U8), UKR_ALPHABET_LOWER_FWD_U8) == 0);	// #MSVC_BUGGED_compile_time_utf8
	static_assert(Mbs::Cmp(Mbs::Lwr(DEU_ALPHABET_UPPER_FWD_U8), DEU_ALPHABET_LOWER_FWD_U8) == 0);	// #MSVC_BUGGED_compile_time_utf8

	static_assert(Wcs::Cmp(WcsR::Lwr(ELL_ALPHABET_UPPER_FWD_W, 10), ELL_ALPHABET_LOWER_BWD_W, 10) == 0);	// Purpose: test 'N' version.
	static_assert(Mbs::Cmp(MbsR::Lwr(UKR_ALPHABET_UPPER_FWD_U8, 10), UKR_ALPHABET_LOWER_BWD_U8, 10) == 0);	// Purpose: test 'N' version.
	static_assert(Mbs::Cmp(MbsR::Lwr(DEU_ALPHABET_UPPER_FWD_U8, 10), DEU_ALPHABET_LOWER_BWD_U8, 10) == 0);	// Purpose: test 'N' version.

	constexpr bool UnitTest_StrLwr() noexcept
	{
		std::string str{ DEU_ALPHABET_UPPER_FWD_U8 };
		MbsR::Lwr(&str);

		return Mbs::Cmp(str, DEU_ALPHABET_LOWER_BWD_U8) == 0;
	}
	static_assert(UnitTest_StrLwr());	// purpose: testing in_place usage.
#endif
}

// PBrk, SpnP, CSpn, Spn
namespace Hydrogenium::String::UnitTest
{
	static_assert(Wcs::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡") == L"葡萄不吐葡萄皮");
	static_assert(Wcs::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡", 1).empty());	// Purpose: test 'N' version.
	static_assert(Mbs::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡") == u8"葡萄不吐葡萄皮");
	static_assert(Mbs::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡", 1).empty());	// Purpose: test 'N' version.
	static_assert(WcsR::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡") == L"葡萄皮");
	static_assert(WcsR::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡", 2).empty());	// Purpose: test 'N' version.
	static_assert(MbsR::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡") == u8"葡萄皮");
	static_assert(MbsR::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡", 2).empty());	// Purpose: test 'N' version.

	static_assert(StrI::PBrk("Try not", "tr") == "Try not");
	static_assert(StrIR::PBrk("Try not", "tr") == "t");
	static_assert(StrI::SpnP("Try not", "tr") == "y not");
	static_assert(StrIR::SpnP("Try not", "tr") == "ot");

	static_assert(MbsRCSpn(u8"吃葡萄不吐葡萄皮", u8"吐葡") == -3);
	//                                 └──┘   ← searching dir
	static_assert(MbsRSpn(u8"吃葡萄不吐葡萄皮", u8"葡萄皮") == -4);
	//                              └────┘    ← searching dir
	static_assert(MbsCSpn("abcde312$#@", "*$#") == 8);
	//                     └───────┘
	static_assert(MbsSpn("abcde312$#@", "qwertyuiopasdfghjklzxcvbnm") == 5);
	//                    └────┘

	// 'N' Version

	static_assert(MbsRCSpn(u8"aäbcdefghijklmnoöpqrsßtuüvwxyz", u8"找不到", 10) == -11);
	//                                           └─────────┘
	static_assert(MbsRCSpn(u8"aäbcdefghijklmnoöpqrsßtuüvwxyz", u8"", 10) == -11);
	//                                           └─────────┘
	static_assert(MbsRCSpn(u8"aäbcdefghijklmnoöpqrsßtuüvwxyz", u8"") == -31);
	//                        └────────────────────────────┘
}

// Rpl
namespace Hydrogenium::String::UnitTest
{
	static_assert(Wcs::Rpl(std::wstring_view{ L"吃葡萄不吐葡萄皮" }, L"葡萄", L"橘子") == L"吃橘子不吐橘子皮");
	static_assert(Wcs::Rpl(std::wstring_view{ L"吃葡萄不吐葡萄皮" }, L"葡萄", L"橘子", 5) == L"吃橘子不吐葡萄皮");
	static_assert(WcsR::Rpl(std::wstring_view{ L"吃葡萄不吐葡萄皮" }, L"葡萄", L"橘子", 5) == L"吃葡萄不吐橘子皮");

	static_assert(StrIR::Rpl(ENG_ALPHABET_UPPER_FWD, ENG_ALPHABET_LOWER_FWD, "123") == "123");
	static_assert(MbsIR::Rpl(ENG_ALPHABET_UPPER_FWD, ENG_ALPHABET_LOWER_FWD, "123") == "123");

#ifdef HYDROGENIUM_UTL_UNICODE
	static_assert(MbsIR::Rpl(DEU_ALPHABET_UPPER_FWD_U8, DEU_ALPHABET_LOWER_FWD_U8, "123") == "123");
	static_assert(MbsIR::Rpl(std::string_view{ u8"HELLO WORLD" }, "LL", "") == "HEO WORLD");
#endif

	constexpr bool UnitTest_Replace()
	{
		std::string sz0{ u8"Teßting caße for ßpecial ẞ in German." };
		MbsI::Rpl(&sz0, u8"ß", u8"s");

		if (sz0 != "Testing case for special s in German.")
			return false;

		sz0 = u8"ẞßẞß";
		MbsI::Rpl(&sz0, u8"ß", u8"s");
		if (sz0 != "ssss")
			return false;

		sz0 = u8"ßẞßẞ";
		MbsI::Rpl(&sz0, u8"ß", u8"s");
		if (sz0 != "ssss")
			return false;

		{
			// Additional spaces for expanding usage.
			char rgsz[64]{ "Testing case for special S in German." };
			MbsI::Rpl(rgsz, "s", u8"ß");

			if (std::string_view{ u8"Teßting caße for ßpecial ß in German." } != rgsz)
				return false;
		}

		{
			char rgsz[]{ u8"Teßting caße for ßpecial ẞ in German." };
			MbsIR::Rpl(rgsz, u8"ß", "S", 12);

			if (std::string_view{ u8"Teßting caße for ßpecial S in German." } != rgsz)
				return false;
		}

		return true;
	}
	static_assert(UnitTest_Replace());
}

// Str
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Str("", "").empty());
	static_assert(Str::Str("", "abc").empty());
	static_assert(Str::Str(ASCII_NUMBERS_FWD, "345", 6) == "345");	// Purpose: test 'N' version.
	static_assert(Str::Str(ASCII_NUMBERS_FWD, "345") == ASCII_NUMBERS_FWD.substr(3));
	static_assert(Str::Str(ASCII_NUMBERS_FWD, "90").empty());	// purpose: this is different from strchr()!

	static_assert(Wcs::Str(L"吃葡萄不吐葡萄皮", L"葡萄") == L"葡萄不吐葡萄皮");
	static_assert(WcsR::Str(L"吃葡萄不吐葡萄皮", L"葡萄") == L"葡萄皮");
	static_assert(Mbs::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄") == u8"葡萄不吐葡萄皮");
	static_assert(MbsR::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄") == u8"葡萄皮");
	static_assert(Mbs::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄", 3) == u8"葡萄");	// purpose: verify the multibytes can be correctly parsed as groups.
	static_assert(MbsR::Str(u8"吃葡萄不吐葡萄皮", u8"葡萄", 3) == u8"葡萄皮");	// Purpose: test 'N' version.

	static_assert(WcsR::Str(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_LOWER_BWD_W).empty());	// purpose: verify the reverse mode has nothing to do with substr dir.
	static_assert(WcsI::Str(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_UPPER_FWD_W.substr(10)) == ELL_ALPHABET_LOWER_FWD_W.substr(10));
	static_assert(WcsI::Str(ELL_ALPHABET_LOWER_FWD_W, ELL_ALPHABET_UPPER_FWD_W.substr(10), 10).empty());	// Purpose: test 'N' version.

	static_assert(MbsR::Str(DEU_ALPHABET_LOWER_FWD_U8, DEU_ALPHABET_LOWER_BWD_U8).empty());	// purpose: verify the reverse mode has nothing to do with substr dir.
	static_assert(MbsI::Str(DEU_ALPHABET_LOWER_FWD_U8, MbsR::Sub(DEU_ALPHABET_UPPER_FWD_U8, 10)) == MbsR::Sub(DEU_ALPHABET_LOWER_FWD_U8, 10));
	static_assert(MbsI::Str(DEU_ALPHABET_LOWER_FWD_U8, MbsR::Sub(DEU_ALPHABET_UPPER_FWD_U8, 10), 10).empty());	// Purpose: test 'N' version.
	static_assert(MbsIR::Str(DEU_ALPHABET_LOWER_FWD_U8, u8"uüv", 7) == u8"uüvwxyz");	// purpose: additional case for IR, just in case.
}

// Tok
namespace Hydrogenium::String::UnitTest
{
	template <typename T>
	inline bool UnitTest_StrTok_ConsecutiveCall(std::ranges::input_range auto&& view, auto&& delim, std::ranges::range auto&& ans) noexcept
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

	template <typename T>
	inline bool UnitTest_StrTok_RangeOfViews(std::ranges::input_range auto&& view, auto&& delim, std::ranges::range auto&& ans) noexcept
	{
		using namespace Hydrogenium::StringPolicy::Result;

		if (!std::ranges::equal(T::Tok(as_vector_t{}, view, delim, 0xFFFF), ans))
			return false;

		if (!std::ranges::equal(T::Tok(as_generator_t{}, view, delim, 0xFFFF), ans))
			return false;

		return true;
	}

	template <typename T>
	inline bool UnitTest_StrTok_AllStyles(std::ranges::input_range auto&& view, auto&& delim, std::ranges::range auto&& ans) noexcept
	{
		return
			UnitTest_StrTok_ConsecutiveCall<T>(view, delim, ans)
			&& UnitTest_StrTok_RangeOfViews<T>(view, delim, ans);
	}

	void UnitTest_StrTok() noexcept
	{
		assert(UnitTest_StrTok_AllStyles<Str>("hello, world", " ,\t", std::vector{ "hello", "world" }));
		assert(UnitTest_StrTok_AllStyles<StrR>("hello, world", " ,\t", std::vector{ "hello", "world" } | std::views::reverse));
		assert(UnitTest_StrTok_AllStyles<Str>("", " ,\t", std::views::empty<std::string_view>));
		assert(UnitTest_StrTok_AllStyles<StrR>("", "", std::views::empty<std::string_view>));
		assert(UnitTest_StrTok_AllStyles<WcsI>(ELL_ALPHABET_UPPER_FWD_W, L"αεηιουω", std::vector{ L"ΒΓΔ", L"Ζ", L"Θ", L"ΚΛΜΝΞ", L"ΠΡ΢ΣΤ", L"ΦΧΨ" }));
		assert(UnitTest_StrTok_AllStyles<WcsIR>(ELL_ALPHABET_UPPER_FWD_W, L"αεηιουω", std::vector{ L"ΒΓΔ", L"Ζ", L"Θ", L"ΚΛΜΝΞ", L"ΠΡ΢ΣΤ", L"ΦΧΨ" } | std::views::reverse));
		assert(UnitTest_StrTok_AllStyles<MbsI>(DEU_ALPHABET_UPPER_FWD_U8, "äeiöü", std::vector{ u8"A", u8"BCD", u8"FGH", u8"JKLMNO", u8"PQRSẞTU", u8"VWXYZ" }));
		assert(UnitTest_StrTok_AllStyles<MbsIR>(DEU_ALPHABET_UPPER_FWD_U8, "äeiöü", std::vector{ u8"A", u8"BCD", u8"FGH", u8"JKLMNO", u8"PQRSẞTU", u8"VWXYZ" } | std::views::reverse));
		assert(Str::Tok(std::nullopt, "").empty());	// purpose: test how the residue from different calls handled.
	}
}

// Trm, originally UTIL_Trim()
namespace Hydrogenium::String::UnitTest
{
	static_assert(Str::Trm("").empty());
	static_assert(Str::Trm(" \r\n\t").empty());
	static_assert(Str::Trm(" abc ") == "abc");
	static_assert(Str::Trm(" abc") == "abc");
	static_assert(Str::Trm("abc ") == "abc");
	static_assert(Str::Trm("abc") == "abc");

	static_assert(Wcs::Trm(L"").empty());
	static_assert(Wcs::Trm(L" \r\n\t").empty());
	static_assert(Wcs::Trm(L" abc ") == L"abc");
	static_assert(Wcs::Trm(L" abc") == L"abc");
	static_assert(Wcs::Trm(L"abc ") == L"abc");
	static_assert(Wcs::Trm(L"abc") == L"abc");

	static_assert(Mbs::Trm(u8"").empty());
	static_assert(Mbs::Trm(u8" \r\n\t").empty());
	static_assert(Mbs::Trm(u8" あいうえお ") == u8"あいうえお");
	static_assert(Mbs::Trm(u8" あいうえお") == u8"あいうえお");
	static_assert(Mbs::Trm(u8"あいうえお ") == u8"あいうえお");
	static_assert(Mbs::Trm(u8"あいうえお") == u8"あいうえお");

	// Purpose: testing in-place mode.
	template <typename U, typename T = std::vector<typename decltype(U::Trm)::owner_type>, typename A = std::vector<typename decltype(U::Trm)::view_type>>
	consteval bool UnitTest_Trim(T rgszTestCases, A rgszAnswers) noexcept
	{
		for (auto&& [szTestCase, szAnswer] : std::views::zip(rgszTestCases, rgszAnswers))
		{
			U::Trm(&szTestCase);

			if (szTestCase != szAnswer)
				return false;
		}

		return true;
	}
	static_assert(UnitTest_Trim<Mbs>(
		{ u8"", u8" \r\n\t",	u8" あいうえお ",	u8" あいうえお",	u8"あいうえお ",	u8"あいうえお", },
		{ u8"", u8"",			u8"あいうえお",	u8"あいうえお",	u8"あいうえお",	u8"あいうえお", }
	));
	static_assert(UnitTest_Trim<Wcs>(
		{ L"", L" \r\n\t",	L" あいうえお ",	L" あいうえお",	L"あいうえお ",	L"あいうえお", },
		{ L"", L"",			L"あいうえお",	L"あいうえお",	L"あいうえお",	L"あいうえお", }
	));
}

// Upr
namespace Hydrogenium::String::UnitTest
{
	static_assert(StrR::Upr(ENG_ALPHABET_LOWER_FWD) == ENG_ALPHABET_UPPER_BWD);
	static_assert(Str::Cmp(StrR::Upr(ENG_ALPHABET_LOWER_FWD, 10), ENG_ALPHABET_UPPER_BWD, 10) == 0);	// Purpose: test 'N' version.

#ifdef HYDROGENIUM_UTL_UNICODE
	static_assert(Mbs::Cmp(MbsR::Upr(UKR_ALPHABET_LOWER_FWD_U8), UKR_ALPHABET_UPPER_BWD_U8) == 0); // #MSVC_BUGGED_compile_time_utf8
	static_assert(Mbs::Cmp(MbsR::Upr(DEU_ALPHABET_LOWER_FWD_U8), DEU_ALPHABET_UPPER_BWD_U8) == 0);
	static_assert(WcsR::Upr(ELL_ALPHABET_LOWER_FWD_W) == ELL_ALPHABET_UPPER_BWD_W);

	static_assert(Wcs::Cmp(WcsR::Upr(ELL_ALPHABET_LOWER_FWD_W, 10), ELL_ALPHABET_UPPER_BWD_W, 10) == 0);	// Purpose: test 'N' version.
	static_assert(Mbs::Cmp(MbsR::Upr(UKR_ALPHABET_LOWER_FWD_U8, 10), UKR_ALPHABET_UPPER_BWD_U8, 10) == 0);	// Purpose: test 'N' version.
	static_assert(Mbs::Cmp(MbsR::Upr(DEU_ALPHABET_LOWER_FWD_U8, 10), DEU_ALPHABET_UPPER_BWD_U8, 10) == 0);	// Purpose: test 'N' version.

	constexpr bool UnitTest_StrUpr() noexcept
	{
		std::string str{ DEU_ALPHABET_LOWER_FWD_U8 };
		MbsR::Upr(&str);

		return Mbs::Cmp(str, DEU_ALPHABET_UPPER_BWD_U8) == 0;
	}
	static_assert(UnitTest_StrUpr());	// purpose: testing in_place usage.
#endif
}
