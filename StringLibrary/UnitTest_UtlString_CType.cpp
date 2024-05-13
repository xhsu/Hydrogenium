#include "UtlString.hpp"

#include <span>

/*
	auto fn =
		[&](std::string_view name, unsigned char first, unsigned char last) noexcept
		{
			fmt::print(f, "inline constexpr unsigned char {0}_C_ARR[] = {{ ", name);
			for (auto i = first; i <= last; ++i)
				fmt::print(f, R"({0:?}, )", (char)i);
			fmt::print(f, "}};\n");

			fmt::print(f, "inline constexpr std::span<unsigned char const> {0} = {{ {0}_C_ARR, }};\n", name);
		};

	fn("CONTROL_CODES_1", 0, 8);
	fn("TAB", 9, 9);
	fn("WHITE_SPACES", 10, 13);
	fn("CONTROL_CODES_2", 14, 31);
	fn("SPACE", 32, 32);
	fn("PUNCTUATIONS_1", 33, 47);
	fn("NUMBERS", 48, 57);
	fn("PUNCTUATIONS_2", 58, 64);
	fn("UPPER_A_TO_F", 65, 70);
	fn("UPPER_G_TO_Z", 71, 90);
	fn("PUNCTUATIONS_3", 91, 96);
	fn("LOWER_A_TO_F", 97, 102);
	fn("LOWER_G_TO_Z", 103, 122);
	fn("PUNCTUATIONS_4", 123, 126);
	fn("BACKSPACE", 127, 127);
	fn("ASCII", 0, 127);

*/

/*

	auto fn =
		[&](std::string_view name, std::span<std::span<unsigned char const> const> rs) noexcept
		{
			std::print(f, "inline constexpr unsigned char {0}[] = {{ ", name);

			for (auto&& r : rs)
			{
				for (auto&& c : r)
				{
					fmt::print(f, "{:?}, ", (char)c);
				}
			}

			std::print(f, "}};\n");
		};

	fn("CHSET_IsCntrl", std::array{ ASCII.subspan(0, 32), ASCII.subspan(127, 1) });
	fn("CHSET_IsPrint", std::array{ ASCII.subspan(32, 95), });
	fn("CHSET_IsSpace", std::array{ ASCII.subspan(9, 5), ASCII.subspan(32, 1), });
	fn("CHSET_IsBlank", std::array{ ASCII.subspan(9, 1), ASCII.subspan(32, 1), });
	fn("CHSET_IsGraph", std::array{ ASCII.subspan(33, 94), });
	fn("CHSET_IsPunct", std::array{ ASCII.subspan(33, 47 - 33 + 1), ASCII.subspan(58, 64 - 58 + 1), ASCII.subspan(91, 6), ASCII.subspan(123, 4), });
	fn("CHSET_IsAlNum", std::array{ ASCII.subspan(48, 10), ASCII.subspan(65, 26), ASCII.subspan(97, 26), });
	fn("CHSET_IsAlpha", std::array{ ASCII.subspan(65, 26), ASCII.subspan(97, 26), });
	fn("CHSET_IsUpper", std::array{ ASCII.subspan(65, 26), });
	fn("CHSET_IsLower", std::array{ ASCII.subspan(97, 26), });
	fn("CHSET_IsDigit", std::array{ ASCII.subspan(48, 10), });
	fn("CHSET_IsXDigit", std::array{ ASCII.subspan(48, 10), ASCII.subspan(65, 6), ASCII.subspan(97, 6), });

*/


// Unit tests for CType regular function wrappers.
namespace Hydrogenium::UnitTest
{
	inline constexpr unsigned char ASCII_C_ARR[] = { '\x00', '\x01', '\x02', '\x03', '\x04', '\x05', '\x06', '\x07', '\x08', '\t', '\n', '\x0b', '\x0c', '\r', '\x0e', '\x0f', '\x10', '\x11', '\x12', '\x13', '\x14', '\x15', '\x16', '\x17', '\x18', '\x19', '\x1a', '\x1b', '\x1c', '\x1d', '\x1e', '\x1f', ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', '\x7f', };
	inline constexpr unsigned char CHSET_IsCntrl[] = { '\x00', '\x01', '\x02', '\x03', '\x04', '\x05', '\x06', '\x07', '\x08', '\t', '\n', '\x0b', '\x0c', '\r', '\x0e', '\x0f', '\x10', '\x11', '\x12', '\x13', '\x14', '\x15', '\x16', '\x17', '\x18', '\x19', '\x1a', '\x1b', '\x1c', '\x1d', '\x1e', '\x1f', '\x7f', };
	inline constexpr unsigned char CHSET_IsPrint[] = { ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', };
	inline constexpr unsigned char CHSET_IsSpace[] = { '\t', '\n', '\x0b', '\x0c', '\r', ' ', };
	inline constexpr unsigned char CHSET_IsBlank[] = { '\t', ' ', };
	inline constexpr unsigned char CHSET_IsGraph[] = { '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', };
	inline constexpr unsigned char CHSET_IsPunct[] = { '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', '\\', ']', '^', '_', '`', '{', '|', '}', '~', };
	inline constexpr unsigned char CHSET_IsAlNum[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', };
	inline constexpr unsigned char CHSET_IsAlpha[] = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', };
	inline constexpr unsigned char CHSET_IsUpper[] = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', };
	inline constexpr unsigned char CHSET_IsLower[] = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', };
	inline constexpr unsigned char CHSET_IsDigit[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', };
	inline constexpr unsigned char CHSET_IsXDigit[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f', };

	consteval bool UnitTest_CTypeFunctionWrapper(auto&& pfn, std::span<unsigned char const> chset)
	{
		for (unsigned char i = 0; i < 128; ++i)
		{
			auto const res = pfn(i);

			if (res != std::ranges::contains(chset, i))
				return false;
		}

		return true;
	}

#define TEST_FN(func) static_assert(UnitTest_CTypeFunctionWrapper(&CType<char>::func, CHSET_##func) && UnitTest_CTypeFunctionWrapper(&CType<wchar_t>::func, CHSET_##func))

	TEST_FN(IsCntrl);
	TEST_FN(IsPrint);
	TEST_FN(IsSpace);
	TEST_FN(IsBlank);
	TEST_FN(IsGraph);
	TEST_FN(IsPunct);
	TEST_FN(IsAlNum);
	TEST_FN(IsAlpha);
	TEST_FN(IsUpper);
	TEST_FN(IsLower);
	TEST_FN(IsDigit);
	TEST_FN(IsXDigit);

#undef TEST_FN
}

// Unit tests for CType but Luna's extensions.
namespace Hydrogenium::UnitTest
{
	static_assert(CType<char>::CodePointOf(u8"A"[0]) == CodePoint::WHOLE);
	static_assert(CType<char>::CodePointOf(u8"Á"[0]) == CodePoint::BEGIN_OF_2);
	static_assert(CType<char>::CodePointOf(u8"あ"[0]) == CodePoint::BEGIN_OF_3);
	static_assert(CType<char>::CodePointOf(u8"あ"[1]) == CodePoint::MID);
	static_assert(CType<char>::CodePointOf(u8"𐒰"[0]) == CodePoint::BEGIN_OF_4);

	static_assert(CType<char>::ToFullWidth(u8"A") == U'A');
	static_assert(CType<char>::ToFullWidth(u8"Á") == U'Á');
	static_assert(CType<char>::ToFullWidth(u8"あ") == U'あ');
	static_assert(CType<char>::ToFullWidth(u8"𐒰") == U'𐒰');

	using U8MBARR = std::array<unsigned char, 4>;
	static_assert(CType<char>::ToMultiBytes(U'A') == U8MBARR{ 'A', 0, 0, 0 });
	static_assert(CType<char>::ToMultiBytes(U'Á') == U8MBARR{ 0xC3, 0x81, 0, 0 });
	static_assert(CType<char>::ToMultiBytes(U'あ') == U8MBARR{ 0xE3, 0x81, 0x82, 0 });
	static_assert(CType<char>::ToMultiBytes(U'𐒰') == U8MBARR{ 0xF0, 0x90, 0x92, 0xB0 });

	static_assert(CType<char16_t>::CodePointOf(u"A"[0]) == CodePoint::WHOLE);
	static_assert(CType<char16_t>::CodePointOf(u"Á"[0]) == CodePoint::WHOLE);
	static_assert(CType<char16_t>::CodePointOf(u"あ"[0]) == CodePoint::WHOLE);
	static_assert(CType<char16_t>::CodePointOf(u"𐒰"[0]) == CodePoint::BEGIN_OF_2);
	static_assert(CType<char16_t>::CodePointOf(u"𐒰"[1]) == CodePoint::MID);

	static_assert(CType<char16_t>::ToFullWidth(u"A") == U'A');
	static_assert(CType<char16_t>::ToFullWidth(u"Á") == U'Á');
	static_assert(CType<char16_t>::ToFullWidth(u"あ") == U'あ');
	static_assert(CType<char16_t>::ToFullWidth(u"𐒰") == U'𐒰');

	constexpr auto operator== (std::array<char16_t, 2> const& lhs, std::array<wchar_t, 2> const& rhs) noexcept
	{
		return lhs[0] == rhs[0] && lhs[1] == rhs[1];
	}

	using U16MBARR = std::array<char16_t, 2>;
	static_assert(CType<char16_t>::ToMultiBytes(U'A') == U16MBARR{ u'A', 0 });
	static_assert(CType<char16_t>::ToMultiBytes(U'Á') == U16MBARR{ u'Á', 0 });
	static_assert(CType<char16_t>::ToMultiBytes(U'あ') == U16MBARR{ u'あ', 0 });
	static_assert(CType<char16_t>::ToMultiBytes(U'𐒰') == U16MBARR{ 0xD801, 0xDCB0 });
}
