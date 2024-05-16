/*
	Created at: May 10 2024
*/

#pragma once

#define HYDROGENIUM_UTL_STRING 20240510L

#include <cassert>
#include <cctype>
#include <cstdint>
#include <cwctype>

#include <algorithm>
#include <concepts>
#include <functional>
#include <limits>
#include <optional>	// Str::Tok()
#include <random>	// Str::detail::Fry()
#include <ranges>
#include <string_view>
#include <typeinfo>
#include <utility>

#if __has_include(<generator>)	// #UPDATE_AT_CPP23 generator
#include <generator>
#define GENERATOR_TY ::std::generator
#elif __has_include(<experimental/generator>)
#include <experimental/generator>
#define GENERATOR_TY ::std::experimental::generator
#elif __has_include(<cppcoro/generator.hpp>)
#include <cppcoro/generator.hpp>
#define GENERATOR_TY ::cppcoro::generator
#endif

#if __has_include("UtlUnicode.hpp")
#include "UtlUnicode.hpp"
#endif



namespace Hydrogenium::UnitTest
{
	inline constexpr std::string_view ASCII_NUMBERS_FWD = "0123456789";
	inline constexpr std::string_view ASCII_NUMBERS_BWD = "9876543210";
	inline constexpr std::string_view CJK_NUMBERS_FWD_U8 = u8"零一二三四五六七八九";
	inline constexpr std::string_view CJK_NUMBERS_BWD_U8 = u8"九八七六五四三二一零";
	inline constexpr std::wstring_view RMN_NUMBERS_FWD_W = L"ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩ";
	inline constexpr std::wstring_view RMN_NUMBERS_BWD_W = L"ⅩⅨⅧⅦⅥⅤⅣⅢⅡⅠ";

	inline constexpr char32_t ASCII_NUMBERS_FWD_U32ARR[] = { U'0', U'1', U'2', U'3', U'4', U'5', U'6', U'7', U'8', U'9', };
	inline constexpr char32_t ASCII_NUMBERS_BWD_U32ARR[] = { U'9', U'8', U'7', U'6', U'5', U'4', U'3', U'2', U'1', U'0', };
	inline constexpr char32_t CJK_NUMBERS_FWD_U32ARR[] = { U'零', U'一', U'二', U'三', U'四', U'五', U'六', U'七', U'八', U'九', };
	inline constexpr char32_t CJK_NUMBERS_BWD_U32ARR[] = { U'九', U'八', U'七', U'六', U'五', U'四', U'三', U'二', U'一', U'零', };
	inline constexpr char32_t RMN_NUMBERS_FWD_U32ARR[] = { U'Ⅰ', U'Ⅱ', U'Ⅲ', U'Ⅳ', U'Ⅴ', U'Ⅵ', U'Ⅶ', U'Ⅷ', U'Ⅸ', U'Ⅹ' };
	inline constexpr char32_t RMN_NUMBERS_BWD_U32ARR[] = { U'Ⅹ', U'Ⅸ', U'Ⅷ', U'Ⅶ', U'Ⅵ', U'Ⅴ', U'Ⅳ', U'Ⅲ', U'Ⅱ', U'Ⅰ' };

	inline constexpr std::string_view ENG_ALPHABET_LOWER_FWD = "abcdefghijklmnopqrstuvwxyz";
	inline constexpr std::string_view ENG_ALPHABET_LOWER_BWD = "zyxwvutsrqponmlkjihgfedcba";
	inline constexpr std::string_view ENG_ALPHABET_UPPER_FWD = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	inline constexpr std::string_view ENG_ALPHABET_UPPER_BWD = "ZYXWVUTSRQPONMLKJIHGFEDCBA";

	inline constexpr std::wstring_view ELL_ALPHABET_LOWER_FWD_W = L"αβγδεζηθικλμνξοπρςστυφχψω";
	inline constexpr std::wstring_view ELL_ALPHABET_LOWER_BWD_W = L"ωψχφυτσςρποξνμλκιθηζεδγβα";
	inline constexpr std::wstring_view ELL_ALPHABET_UPPER_FWD_W = L"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡ΢ΣΤΥΦΧΨΩ";	// Reserved character at 0x3A2
	inline constexpr std::wstring_view ELL_ALPHABET_UPPER_BWD_W = L"ΩΨΧΦΥΤΣ΢ΡΠΟΞΝΜΛΚΙΘΗΖΕΔΓΒΑ";	// Reserved character at 0x3A2

	inline constexpr std::string_view UKR_ALPHABET_LOWER_FWD_U8 = u8"абвгґдеєжзиіїйклмнопрстуфхцчшщьюя";
	inline constexpr std::string_view UKR_ALPHABET_LOWER_BWD_U8 = u8"яюьщшчцхфутсрпонмлкйїіизжєедґгвба";
	inline constexpr std::string_view UKR_ALPHABET_UPPER_FWD_U8 = u8"АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ";
	inline constexpr std::string_view UKR_ALPHABET_UPPER_BWD_U8 = u8"ЯЮЬЩШЧЦХФУТСРПОНМЛКЙЇІИЗЖЄЕДҐГВБА";

	inline constexpr std::string_view DEU_ALPHABET_LOWER_FWD_U8 = u8"aäbcdefghijklmnoöpqrsßtuüvwxyz";
	inline constexpr std::string_view DEU_ALPHABET_LOWER_BWD_U8 = u8"zyxwvüutßsrqpöonmlkjihgfedcbäa";
	inline constexpr std::string_view DEU_ALPHABET_UPPER_FWD_U8 = u8"AÄBCDEFGHIJKLMNOÖPQRSẞTUÜVWXYZ";
	inline constexpr std::string_view DEU_ALPHABET_UPPER_BWD_U8 = u8"ZYXWVÜUTẞSRQPÖONMLKJIHGFEDCBÄA";
}

namespace Hydrogenium
{
	template <typename T>
	concept NonVoid = !std::is_same_v<T, void>;

	template <typename T>
	concept ReverseIterator = requires (T iter)
	{
		iter.base();
		{ iter.base() } -> std::bidirectional_iterator;
		{ *iter.base() } -> std::same_as<decltype(*iter)>;
	};

	template <typename T>
	concept MultiWrappedRevIter = requires (T iter)
	{
		iter.base();
		{ iter.base() } -> ReverseIterator;
	};

	// Pointing to same elem!! Recursively remove all wrapped r_iter<>
	constexpr auto ToForwardIter(auto iter, bool do_offset = true) noexcept
	{
		if constexpr (MultiWrappedRevIter<decltype(iter)>)
		{
			return ToForwardIter(iter.base());
		}
		else if constexpr (ReverseIterator<decltype(iter)>)
		{
			if (do_offset)
			{
				// &*(reverse_iterator(i)) == &*(i - 1)
				return (++iter).base();
			}
			else
				return iter.base();
		}
		else
		{
			return iter;
		}
	}

	// Pointing to same elem!! Recursively remove all wrapped r_iter<>
	constexpr auto ToReverseIter(auto iter, bool do_offset = true) noexcept
	{
		if constexpr (MultiWrappedRevIter<decltype(iter)>)
		{
			return ToReverseIter(iter.base());
		}
		else if constexpr (ReverseIterator<decltype(iter)>)
		{
			return iter;
		}
		else
		{
			if (do_offset)
			{
				// &*(reverse_iterator(i)) == &*(i - 1)
				return std::make_reverse_iterator(iter) - 1;
			}
			else
				return std::make_reverse_iterator(iter);
		}
	}

	// Unit testing at: UnitTest_UtlString_Misc.cpp
}

namespace Hydrogenium
{
	enum struct CodePoint : uint_fast8_t
	{
		WHOLE = 1,
		BEGIN_OF_2 = 2,
		BEGIN_OF_3 = 3,
		BEGIN_OF_4 = 4,
		MID,
		INVALID,
	};

	constexpr auto operator<=> (CodePoint lhs, CodePoint rhs) noexcept
	{
		return std::to_underlying(lhs) <=> std::to_underlying(rhs);
	}

	// Full Unicode character type
	using fchar_t = std::conditional_t<sizeof(wchar_t) == sizeof(char32_t), wchar_t, char32_t>;

	// #TODO UTF32 a.k.a. all-lang support?

	template <typename T>
	struct CType final
	{
		using char_type = std::remove_cvref_t<T>;
		static inline constexpr bool is_char_type =
			std::is_same_v<char_type, char> || std::is_same_v<char_type, signed char> || std::is_same_v<char_type, unsigned char>
#ifdef __cpp_char8_t
			|| std::is_same_v<char_type, char8_t>
#endif
			|| std::is_same_v<char_type, wchar_t> || std::is_same_v<char_type, char16_t>
			|| std::is_same_v<char_type, char32_t>;
		static_assert(is_char_type, "Must be one of char, signed char, unsigned char, char8_t, char16_t, wchar_t, char32_t.");

		static inline constexpr bool is_narrow = sizeof(char_type) == sizeof(char);
		static inline constexpr bool is_wide = sizeof(char_type) == sizeof(wchar_t);
		static inline constexpr bool no_builtin_ctype_support = !is_narrow && !is_wide;

		static inline constexpr bool is_utf8 = sizeof(char_type) == sizeof(unsigned char);
		static inline constexpr bool is_utf16 = sizeof(char_type) == sizeof(char16_t);
		static inline constexpr bool is_utf32 = sizeof(char_type) == sizeof(char32_t);

		using param_type = std::conditional_t<is_narrow, unsigned char, std::conditional_t<is_wide, wchar_t, std::conditional_t<is_utf8, unsigned char, std::conditional_t<is_utf16, char16_t, std::conditional_t<is_utf32, char32_t, void>>>>>;
		using eof_type = std::common_type_t<decltype(EOF), decltype(WEOF)>;
		using view_type = std::basic_string_view<char_type>;
		using owner_type = std::basic_string<char_type>;
		using traits_type = ::std::char_traits<char_type>;
		using multibytes_type = std::conditional_t<is_narrow, std::array<param_type, 4>, std::conditional_t<is_wide, std::array<param_type, 2>, void>>;
		using mutable_span_t = std::span<char_type>;
		using const_span_t = std::span<std::add_const_t<char_type>>;

		static inline constexpr eof_type eof = is_narrow ? EOF : WEOF;

		//int isalnum(int c);
		//int iswalnum( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsAlNum(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return
					('0' <= c && c <= '9')
					|| ('a' <= c && c <= 'z')
					|| ('A' <= c && c <= 'Z');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isalnum(c);
				else if constexpr (is_wide)
					return std::iswalnum(c);
			}
		}

		//int isalpha(int c);
		//int iswalpha( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsAlpha(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return
					('a' <= c && c <= 'z')
					|| ('A' <= c && c <= 'Z');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isalpha(c);
				else if constexpr (is_wide)
					return std::iswalpha(c);
			}
		}

		//int isblank(int c);
		//int iswblank( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsBlank(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return c == '\t' || c == ' ';
			}
			else
			{
				if constexpr (is_narrow)
					return std::isblank(c);
				else if constexpr (is_wide)
					return std::iswblank(c);
			}
		}

		//int iscntrl(int c);
		//int iswcntrl( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsCntrl(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return
					('\x00' <= c && c <= '\x1F')
					|| c == '\x7F';
			}
			else
			{
				if constexpr (is_narrow)
					return std::iscntrl(c);
				else if constexpr (is_wide)
					return std::iswcntrl(c);
			}
		}

		//int isdigit(int c);
		//int iswdigit( wint_t ch );
		[[nodiscard]] static constexpr bool IsDigit(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return ('0' <= c && c <= '9');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isdigit(c);
				else if constexpr (is_wide)
					return std::iswdigit(c);
			}
		}

		//int isgraph(int c);
		//int iswgraph( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsGraph(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return ('\x21' <= c && c <= '\x7E');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isgraph(c);
				else if constexpr (is_wide)
					return std::iswgraph(c);
			}
		}

		//int islower(int c);
		//int iswlower( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsLower(param_type c) noexcept
		{
#ifndef HYDROGENIUM_UTL_UNICODE
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return 'a' <= c && c <= 'z';
			}
			else
			{
				if constexpr (is_narrow)
					return std::islower(c);
				else if constexpr (is_wide)
					return std::iswlower(c);
			}
#else
			// If it changed after cast it to upper, than it is a lower case.
			// Handled situations like Japanese Kana, different but not upper-lower relation.
			return c != ToUpper(c);
#endif
		}

		//int isprint(int c);
		//int iswprint(std::wint_t ch);
		[[nodiscard]] static constexpr bool IsPrint(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return ('\x20' <= c && c <= '\x7E');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isprint(c);
				else if constexpr (is_wide)
					return std::iswprint(c);
			}
		}

		//int ispunct(int c);
		//int iswpunct( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsPunct(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return
					('\x21' <= c && c <= '\x2F')		// !"#$%&'()*+,-./
					|| ('\x3A' <= c && c <= '\x40')		// :;<=>?@
					|| ('\x5B' <= c && c <= '\x60')		// [\]^_`
					|| ('\x7B' <= c && c <= '\x7E')		// {|}~
					;
			}
			else
			{
				if constexpr (is_narrow)
					return std::ispunct(c);
				else if constexpr (is_wide)
					return std::iswpunct(c);
			}
		}

		//int isspace(int c);
		//int iswspace( wint_t ch );
		[[nodiscard]] static constexpr bool IsSpace(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return
					c == ' '
					|| c == '\f'
					|| c == '\n'
					|| c == '\r'
					|| c == '\t'
					|| c == '\v'
					;
			}
			else
			{
				if constexpr (is_narrow)
					return std::isspace(c);
				else if constexpr (is_wide)
					return std::iswspace(c);
			}
		}

		//int isupper(int c);
		//int iswupper( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsUpper(param_type c) noexcept
		{
#ifndef HYDROGENIUM_UTL_UNICODE
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return 'A' <= c && c <= 'Z';
			}
			else
			{
				if constexpr (is_narrow)
					return std::isupper(c);
				else if constexpr (is_wide)
					return std::iswupper(c);
			}
#else
			// If it changed after cast it to lower, than it is a upper case.
			// Handled situations like Japanese Kana, different but not upper-lower relation.
			return c != ToLower(c);
#endif
		}

		//int isxdigit(int c);
		//int iswxdigit( wint_t ch );
		[[nodiscard]] static constexpr bool IsXDigit(param_type c) noexcept
		{
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				return
					('0' <= c && c <= '9')
					|| ('a' <= c && c <= 'f')
					|| ('A' <= c && c <= 'F');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isxdigit(c);
				else if constexpr (is_wide)
					return std::iswxdigit(c);
			}
		}

		//int tolower(int c);
		//std::wint_t towlower( std::wint_t ch );
		[[nodiscard]] static constexpr auto ToLower(param_type c) noexcept -> decltype(c)
		{
#ifndef HYDROGENIUM_UTL_UNICODE
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				if ('A' <= c && c <= 'Z')
					return static_cast<decltype(c)>(c - 'A' + 'a');

				return c;
			}
			else
			{
				if constexpr (is_narrow)
					return static_cast<decltype(c)>(std::tolower(c));
				else if constexpr (is_wide)
					return static_cast<decltype(c)>(std::towlower(c));
			}
#else
			return static_cast<param_type>(Hydrogenium::Unicode::ToLower_Source1(c));
#endif
		}

		//int toupper(int c);
		//std::wint_t towupper( std::wint_t ch );
		[[nodiscard]] static constexpr auto ToUpper(param_type c) noexcept -> decltype(c)
		{
#ifndef HYDROGENIUM_UTL_UNICODE
			if (std::is_constant_evaluated() || no_builtin_ctype_support)
			{
				if ('a' <= c && c <= 'z')
					return static_cast<decltype(c)>(c - 'a' + 'A');

				return c;
			}
			else
			{
				if constexpr (is_narrow)
					return static_cast<decltype(c)>(std::toupper(c));
				else if constexpr (is_wide)
					return static_cast<decltype(c)>(std::towupper(c));
			}
#else
			return static_cast<param_type>(Hydrogenium::Unicode::ToUpper_Source1(c));
#endif
		}

		// Luna's extension
		[[nodiscard]] static constexpr auto CodePointOf(param_type c) noexcept -> CodePoint
		{
			auto const u = static_cast<uint32_t>(c);

			// UTF-8
			if constexpr (is_utf8)
			{
				if (u <= 0x7F)
					return CodePoint::WHOLE;

				else if ((u & 0b111'000'00) == 0b110'000'00)
					return CodePoint::BEGIN_OF_2;

				else if ((u & 0b1111'0000) == 0b1110'0000)
					return CodePoint::BEGIN_OF_3;

				else if ((u & 0b11111'000) == 0b11110'000)
					return CodePoint::BEGIN_OF_4;

				else if ((u & 0b11'000000) == 0b10'000000)
					return CodePoint::MID;

				else
					return CodePoint::INVALID;
			}

			// UTF-16
			else if constexpr (is_utf16)
			{
				if (!((u - 0xd800u) < 0x800u))
				{
					return CodePoint::WHOLE;
				}
				else if ((u & 0xfffffc00u) == 0xd800u)
				{
					return CodePoint::BEGIN_OF_2;
				}
				else if ((u & 0xfffffc00u) == 0xdc00u)
				{
					return CodePoint::MID;
				}
				else
				{
					return CodePoint::INVALID;
				}
			}

			// UTF-32
			else if constexpr (is_utf32)
			{
				if (u > 0x10FFFFu)
					return CodePoint::INVALID;

				return CodePoint::WHOLE;
			}
		}

		// Luna's extension
		[[nodiscard]] static constexpr auto ToFullWidth(view_type bytes) noexcept -> fchar_t
		{
			switch (CodePointOf(bytes.front()))
			{
			case CodePoint::WHOLE:
				return static_cast<fchar_t>(bytes.front());

			case CodePoint::BEGIN_OF_2:
			{
				assert(bytes.size() == 2);

				if constexpr (is_utf8)
				{
					fchar_t ret = (bytes[0] & 0b00011111) << 6 | (bytes[1] & 0b00111111);

					if (ret < 0x80u)			// Not a valid result, Wrong encoding
						ret = 0;				// Out of UTF8 bound, skip data  
					else if (ret > 0x7FFu)		// Not a valid result, Wrong encoding
						ret = 0;				// Out of UTF8 bound, skip data

					return ret;
				}
				else if constexpr (is_utf16)
				{
					return ((uint16_t)bytes[0] << 10) + (uint16_t)bytes[1] - 0x35FDC00u;
				}

				[[fallthrough]];
			}

			case CodePoint::BEGIN_OF_3:
			{
				if constexpr (is_utf8)
				{
					assert(bytes.size() == 3);
					fchar_t ret = (bytes[0] & 0b00001111) << 12 | (bytes[1] & 0b00111111) << 6 | (bytes[2] & 0b00111111);

					if (ret < 0x800u)			// Not a valid result, Wrong encoding
						ret = 0;				// Out of UTF8 bound, skip data  
					else if (ret > 0xFFFFu)		// Not a valid result, Wrong encoding
						ret = 0;				// Out of UTF8 bound, skip data  

					return ret;
				}

				[[fallthrough]];
			}

			case CodePoint::BEGIN_OF_4:
			{
				if constexpr (is_utf8)
				{
					assert(bytes.size() == 4);
					fchar_t ret =
						(bytes[0] & 0b00000111) << 18 | (bytes[1] & 0b00111111) << 12 | (bytes[2] & 0b00111111) << 6 | (bytes[3] & 0b00111111);

					if (ret < 0x10000u)			// Not a valid result, Wrong encoding
						ret = 0;				// Out of UTF8 bound, skip data  
					else if (ret > 0x10FFFFu)	// Not a valid result, Wrong encoding 
						ret = 0;				// Out of UTF8 bound, skip data  

					return ret;
				}

				[[fallthrough]];
			}

			default:
				assert(false);
				std::unreachable();
			}
		}

		// Luna's extension
		[[nodiscard]] static constexpr auto ToFullWidth(std::integral auto c) noexcept -> fchar_t
		{
			switch (CodePointOf(c))
			{
			case CodePoint::WHOLE:
				return static_cast<fchar_t>(c);

			default:
				assert(false);
				std::unreachable();
			}
		}

		// Luna's extension
		[[nodiscard]] static constexpr auto ToMultiBytes(fchar_t wc) noexcept -> std::pair<multibytes_type, int32_t> requires (is_utf8 || is_utf16)
		{
			std::pair<multibytes_type, int32_t> res{};
			auto& ret = res.first;
			auto& cnt = res.second;

			if constexpr (is_utf8)
			{
				if (wc <= 0x7Fu)
				{
					ret[0] = static_cast<param_type>(wc);

					cnt = 1;
				}
				else if (wc <= 0x7FFu)
				{
					ret[0] = static_cast<param_type>(0xC0u | (wc >> 6));			/* 110xxxxx */
					ret[1] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */

					cnt = 2;
				}
				else if (wc <= 0xFFFFu) {
					ret[0] = static_cast<param_type>(0xE0u | (wc >> 12));			/* 1110xxxx */
					ret[1] = static_cast<param_type>(0x80u | ((wc >> 6) & 0x3Fu));	/* 10xxxxxx */
					ret[2] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */

					cnt = 3;
				}
				else if (wc <= 0x10FFFFu) {
					ret[0] = static_cast<param_type>(0xF0u | (wc >> 18));			/* 11110xxx */
					ret[1] = static_cast<param_type>(0x80u | ((wc >> 12) & 0x3Fu));	/* 10xxxxxx */
					ret[2] = static_cast<param_type>(0x80u | ((wc >> 6) & 0x3Fu));	/* 10xxxxxx */
					ret[3] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */

					cnt = 4;
				}
				else
					cnt = -1;	// out of UTF range
			}
			else if constexpr (is_utf16)
			{
				if (wc < 0x10000u)
				{
					ret[0] = static_cast<param_type>(wc);

					cnt = 1;
				}
				else if (wc <= 0x10FFFFu)
				{
					ret[0] = static_cast<param_type>(((((uint32_t)wc - 0x10000u) << 12) >> 22) + 0xD800u);
					ret[1] = static_cast<param_type>(((((uint32_t)wc - 0x10000u) << 22) >> 22) + 0xDC00u);

					cnt = 2;
				}
				else
					cnt = -1;	// out of UTF range
			}
			else
			{
				assert(false);
				cnt = -2;
			}

			return res;
		}
	};

	constexpr auto BuildStringView(auto&& it1, auto&& it2, auto&& end, bool do_offset) noexcept
		requires (typeid(*it1) == typeid(*it2) && typeid(*end) == typeid(*it2))
	{
		using char_type = std::remove_cvref_t<decltype(*it1)>;
		using fwd_iter_t = decltype(ToForwardIter(it1));

		fwd_iter_t fwit1{}, fwit2{};

		if constexpr (requires {it1 != end; })
			fwit1 = ToForwardIter(it1, it1 != end && do_offset);
		else
			fwit1 = ToForwardIter(it1, do_offset);

		if constexpr (requires {it2 != end; })
			fwit2 = ToForwardIter(it2, it2 != end && do_offset);
		else
			fwit2 = ToForwardIter(it2, do_offset);



		if (fwit1 < fwit2)
			return typename CType<char_type>::view_type{ fwit1, fwit2 };

		return typename CType<char_type>::view_type{ fwit2, fwit1 };
	}

	// Unit testing at: UnitTest_UtlString_CType.cpp
}

namespace Hydrogenium::StringPolicy
{
	namespace Iterating
	{
		// Forward declearation.
		enum struct APRES : std::uint_fast8_t;
	}

	struct MyDummy2
	{
		static constexpr auto Begin(auto&& view) noexcept { return view.begin(); }
		static constexpr auto End(auto&& view) noexcept { return view.end(); }
	};

	template <typename T, typename C>
	concept IteratingPolicy = requires (C* p, C const* cp, C* const pc, CType<C>::view_type view)
	{
		T::normal_pointer;	// enables random-access-iter optimization.
		T::Get(view, MyDummy2{}, ptrdiff_t{});	// the middle one should be range policy.

		{ T::ValueOf(cp) } -> NonVoid;
		{ T::Arithmetic(p, pc, pc, ptrdiff_t{}) } -> std::same_as<Iterating::APRES>;
		{ T::NativeSize(p, pc) } -> std::integral;

		requires !requires{ T::Initialize(std::declval<C*>(), pc, pc); }; // must not be able to handle xvalue or rvalue, as this is treat as if lvalue increment.
		requires !requires{ T::Arithmetic(std::declval<C*>(), pc, pc, ptrdiff_t{}); };
		{ T::ArithCpy(std::declval<C*>(), pc, pc, ptrdiff_t{}) } -> std::same_as<C*>;	// This one gives you a copy.
	};

	template <typename T, typename C>
	concept ComparingPolicy = requires (C c)
	{
		{ T{}.Cmp(c, c) } -> std::same_as<int>;
		{ T{}.Eql(c, c) } -> std::same_as<bool>;
	};

	template <typename T, typename C>
	concept DirectionPolicy = requires (CType<C>::view_type view)
	{
		{ T::Begin(view) } -> std::bidirectional_iterator;
		{ T::End(view) } -> std::bidirectional_iterator;
		{ T::Begin(view) } -> std::input_iterator;
		{ T::End(view) } -> std::input_iterator;
		T::is_reverse;
	};

	// Result post-processing
	template <typename R, typename C>
	concept ResultProcessor = requires (CType<C>::view_type sv, CType<C>::owner_type s)
	{
		R::Query(sv, sv, nullptr);	// it seems like there is no way we can fit a generalized 'iter policy' in.
		R::Test(int{});
		R::Counting(size_t{});
		{ R::Modify(s) } -> std::convertible_to<std::basic_string_view<C>>;
	};
}

namespace Hydrogenium::StringPolicy::Iterating
{
	// #UPDATE_AT_CPP23 static operator()

	enum struct APRES : std::uint_fast8_t
	{
		ADVANCED = (1 << 0),
		RECEDED = (1 << 1),
		MOVED = ADVANCED | RECEDED,

		BOS = (1 << 2),	// begin of string
		EOS = (1 << 3),	// end of string
		NOP = (1 << 4),	// no-operation
		STAYED = BOS | EOS | NOP,

		BAD_MB_POINT = (1 << 5),
	};

	constexpr auto operator| (APRES lhs, APRES rhs) noexcept { return std::to_underlying(lhs) | std::to_underlying(rhs); }
	constexpr auto operator& (APRES lhs, APRES rhs) noexcept { return std::to_underlying(lhs) & std::to_underlying(rhs); }
	constexpr auto operator~ (APRES val) noexcept { return ~std::to_underlying(val); }

	struct as_normal_ptr_t final
	{
		static inline constexpr bool normal_pointer = true;

		static constexpr auto Get(std::ranges::input_range auto&& view, auto RangePolicy, ptrdiff_t size) noexcept
		{
			auto abs_begin = RangePolicy.Begin(view);
			auto abs_end = RangePolicy.End(view);
			auto logical_end = ArithCpy(abs_begin, abs_begin, abs_end, size);

			return std::make_tuple(abs_begin, abs_begin, std::move(logical_end));
		}

		static constexpr auto ValueOf(auto&& iter) noexcept
		{
			using CT = CType<decltype(*iter)>;	// #MSVC_BUGGED_tailing_return_type_namespace_error
			return static_cast<CT::param_type>(*iter);
		}

		static constexpr APRES Arithmetic(auto& iter, auto&& begin, auto&& end, ptrdiff_t num) noexcept
		{
			if (num > 0)
			{
				num = std::min(num, end - iter);
				iter += num;

				return iter >= end ? APRES::EOS : APRES::ADVANCED;
			}
			else if (num < 0)
			{
				num = std::max(num, begin - iter);
				iter += num;

				return iter <= begin ? APRES::BOS : APRES::RECEDED;
			}
			else
				return APRES::NOP;

			std::unreachable();
		}

		[[nodiscard]] static constexpr auto ArithCpy(auto iter, auto&& begin, auto&& end, ptrdiff_t num) noexcept -> decltype(iter)
		{
			Arithmetic(iter, begin, end, num);
			return iter;
		}

		static constexpr ptrdiff_t NativeSize(auto&& iter, auto&& end) noexcept
		{
			return end - iter;
		}
	};

	inline constexpr auto as_regular_ptr = as_normal_ptr_t{};

	struct as_multibytes_t final
	{
		/*
		Consider this case:

			 ++iter →                          ← r_iter++
			 ┌──'Á'──┐ ┌───────'𐒰'───────┐ ┌────'あ'────┐
		0x?? 0xC3 0x81 0xF0 0x90 0x92 0xB0 0xE3 0x81 0x82 0x00
		┊    |              |                        ┊    |
		┊    begin(min)     Your iterator.           ┊    end(max)
		rend(max)                                    rbegin(min)

		One should move this iterator to the position of 0xF0, it then would be reasonable to move to any other point.
		*/

		static inline constexpr bool normal_pointer = false;

		// Not required functions
		struct detail final
		{
			static constexpr APRES Advance(auto& iter, auto&& end) noexcept
			{
				using CT = CType<decltype(*iter)>;

				using iter_t = std::remove_cvref_t<decltype(iter)>;
				//using bgn_t = std::remove_cvref_t<decltype(begin)>;
				using ed_t = std::remove_cvref_t<decltype(end)>;

				constexpr bool ALL_NORM_ITER = !ReverseIterator<iter_t> /*&& !ReverseIterator<bgn_t>*/ && !ReverseIterator<ed_t>;
				constexpr bool ALL_REV_ITER = ReverseIterator<iter_t> /*&& ReverseIterator<bgn_t>*/ && ReverseIterator<ed_t>;

				static_assert(ALL_NORM_ITER || ALL_REV_ITER);

				// ASSUMES:
				//	I.		not empty range.
				//	II.		points at a beginning of a UTF stream.
				//	III.	iter != end

				if constexpr (ALL_NORM_ITER)
				{
					auto const cp = CT::CodePointOf(*iter);

					switch (cp)
					{
					case CodePoint::WHOLE:
					case CodePoint::BEGIN_OF_2:
					case CodePoint::BEGIN_OF_3:
					case CodePoint::BEGIN_OF_4:
						iter += std::to_underlying(cp);
						break;

					default:
						assert(false);
						std::unreachable();
					}

					if (iter >= end)
					{
						iter = end;
						return APRES::EOS;
					}

					return APRES::ADVANCED;
				}
				else if constexpr (ALL_REV_ITER)
				{
					if (iter >= end)
					{
						iter = end;
						return APRES::EOS;
					}

					do
					{
						++iter;
					} while (iter < end && CT::CodePointOf(*iter) >= CodePoint::MID);

					return iter >= end ? APRES::EOS : APRES::ADVANCED;
				}

				std::unreachable();
			}

			static constexpr APRES Recede(auto& iter, auto&& begin, auto&& end) noexcept
			{
				using CT = CType<decltype(*iter)>;

				using iter_t = std::remove_cvref_t<decltype(iter)>;
				using bgn_t = std::remove_cvref_t<decltype(begin)>;
				using ed_t = std::remove_cvref_t<decltype(end)>;

				constexpr bool ALL_NORM_ITER = !ReverseIterator<iter_t> && !ReverseIterator<bgn_t> && !ReverseIterator<ed_t>;
				constexpr bool ALL_REV_ITER = ReverseIterator<iter_t> && ReverseIterator<bgn_t> && ReverseIterator<ed_t>;

				static_assert(ALL_NORM_ITER || ALL_REV_ITER);

				// ASSUMES:
				//	I.		not empty range.
				//	II.		points at a beginning of a UTF stream.
				//	III.	iter != end

				if constexpr (ALL_NORM_ITER)
				{
					if (iter <= begin)
					{
						iter = begin;
						return APRES::BOS;
					}

					do
					{
						--iter;
					} while (iter > begin && CT::CodePointOf(*iter) >= CodePoint::MID);

					return APRES::RECEDED;
				}
				else if constexpr (ALL_REV_ITER)
				{
					if (iter <= begin)
					{
						iter = begin;
						Initialize(iter, begin, end);
						return APRES::BOS;
					}

					if (iter >= end)
					{
						// the end cannot be dereferenced, just iterate until a valid pos found.
						do 
						{
							--iter;
						} while (iter > begin && CT::CodePointOf(*iter) >= CodePoint::MID);

						return iter == begin ? APRES::BOS : APRES::RECEDED;
					}

					auto const cp = CT::CodePointOf(*iter);
					auto const cells_to_move = std::to_underlying(cp);

					if (auto const diff = iter - begin; diff < cells_to_move)
					{
						// Ref to the chart at the beginning of this class.
						// There is no 'next' in this case.
						return APRES::BOS;
					}

					switch (cp)
					{
					case CodePoint::WHOLE:
					case CodePoint::BEGIN_OF_2:
					case CodePoint::BEGIN_OF_3:
					case CodePoint::BEGIN_OF_4:
						iter -= cells_to_move;
						break;

					default:
						assert(false);
						std::unreachable();
					}

					return APRES::RECEDED;
				}

				std::unreachable();
			}

			// Not matter what direction it is, this function always put iter onto the head of UTF sequence. Unless it's 'end'
			static constexpr APRES Initialize(auto& iter, auto&& begin, auto&& end) noexcept
			{
				using CT = CType<decltype(*iter)>;

				using iter_t = std::remove_cvref_t<decltype(iter)>;
				using bgn_t = std::remove_cvref_t<decltype(begin)>;
				using ed_t = std::remove_cvref_t<decltype(end)>;

				constexpr bool ALL_NORM_ITER = !ReverseIterator<iter_t> && !ReverseIterator<bgn_t> && !ReverseIterator<ed_t>;
				constexpr bool ALL_REV_ITER = ReverseIterator<iter_t> && ReverseIterator<bgn_t> && ReverseIterator<ed_t>;

				static_assert(ALL_NORM_ITER || ALL_REV_ITER);

				if (begin == end)
					return APRES::NOP;

				// if iter == end, nothing will be done.
				// the end iter was assumed to NEVER be able to dereference.

				if constexpr (ALL_NORM_ITER)
				{
					if (iter == begin)
					{
						auto const cp = CT::CodePointOf(*iter);

						if (cp >= CodePoint::MID)
							return APRES::BAD_MB_POINT;

						return APRES::BOS;
					}

					if (iter == end)
					{
						/*do
						{
							--iter;

							if (auto const cp = CT::CodePointOf(*iter); cp <= CodePoint::BEGIN_OF_4)
								break;

						} while (iter > begin);

						return APRES::RECEDED;*/
						return APRES::NOP;
					}

					while (CT::CodePointOf(*iter) >= CodePoint::MID)
					{
						--iter;
					}

					return APRES::RECEDED;
				}
				else if constexpr (ALL_REV_ITER)
				{
					if (iter == begin)
					{
						if (auto const cp = CT::CodePointOf(*iter); cp != CodePoint::WHOLE && cp != CodePoint::MID)
							return APRES::BAD_MB_POINT;

						while (iter < end && CT::CodePointOf(*iter) > CodePoint::BEGIN_OF_4)
						{
							++iter;	// why increment? check the graph at the beginning of this class.
						}

						// the return value is stating in related to iterator, not human-language nor UTF encoding.
						return APRES::ADVANCED;
					}

					else if (iter == end)
					{
						/*do
						{
							--iter;

							if (auto const cp = CT::CodePointOf(*iter); cp <= CodePoint::BEGIN_OF_4)
								break;

						} while (iter > begin);

						return APRES::RECEDED;*/
						return APRES::NOP;
					}

					while (CT::CodePointOf(*iter) >= CodePoint::MID)
					{
						// we are expecting the head of UTF-8 sequence at 'last' if it is a reverse iterator.
						++iter;
					}

					return APRES::ADVANCED;
				}

				std::unreachable();
			}
		};
		// End of not required functions

		static constexpr auto Get(std::ranges::input_range auto&& view, auto RangePolicy, ptrdiff_t size) noexcept
		{
			auto abs_begin = RangePolicy.Begin(view);
			auto const abs_end = RangePolicy.End(view);

			auto it = abs_begin;
			detail::Initialize(it, abs_begin, abs_end);

			auto logical_end = ArithCpy(it, abs_begin, abs_end, size);

			return std::make_tuple(std::move(abs_begin), std::move(it), std::move(logical_end));
		}

		static constexpr auto ValueOf(auto&& iter) noexcept -> fchar_t
		{
			using CT = CType<decltype(*iter)>;

			auto const fwd_iter = ToForwardIter(iter);
			auto const cp = CT::CodePointOf(*fwd_iter);

			switch (cp)
			{
			case CodePoint::WHOLE:
			case CodePoint::BEGIN_OF_2:
			case CodePoint::BEGIN_OF_3:
			case CodePoint::BEGIN_OF_4:
				return CT::ToFullWidth({ fwd_iter, fwd_iter + std::to_underlying(cp) });

			default:
				std::abort();
			}

			std::unreachable();
		}

		// Iter argument must be pointing to head of UTF stream, begin, or end.
		static constexpr APRES Arithmetic(auto& iter, auto&& begin, auto&& end, ptrdiff_t num) noexcept
		{
			using iter_t = std::remove_cvref_t<decltype(iter)>;
			using bgn_t = std::remove_cvref_t<decltype(begin)>;
			using ed_t = std::remove_cvref_t<decltype(end)>;

			constexpr bool ALL_NORM_ITER = !ReverseIterator<iter_t> && !ReverseIterator<bgn_t> && !ReverseIterator<ed_t>;
			constexpr bool ALL_REV_ITER = ReverseIterator<iter_t> && ReverseIterator<bgn_t> && ReverseIterator<ed_t>;

			static_assert(ALL_NORM_ITER || ALL_REV_ITER);

			// ASSUMES:
			// I. iterator is pointing to the beginning of a UTF stream.

			if (begin == end)
				return APRES::NOP;

			if (num > 0)
			{
				for (; num > 0; --num)
				{
					if (detail::Advance(iter, end) == APRES::EOS)
						return APRES::EOS;
				}

				return APRES::ADVANCED;
			}
			else if (num < 0)
			{
				for (; num < 0; ++num)
				{
					if (detail::Recede(iter, begin, end) == APRES::BOS)
						return APRES::BOS;
				}

				return APRES::RECEDED;
			}
			else
				return APRES::NOP;

			std::unreachable();
		}

		[[nodiscard]]
		static constexpr auto ArithCpy(auto iter, auto&& begin, auto&& end, ptrdiff_t num) noexcept -> decltype(iter)
		{
			Arithmetic(iter, begin, end, num);
			return iter;
		}

		// 'iter' must pointing to head of UTF stream, or end.
		static constexpr ptrdiff_t NativeSize(auto iter, auto&& end) noexcept
		{
			if (iter == end)
				return 0;

			using CT = CType<decltype(*iter)>;

			ptrdiff_t ret{ std::to_underlying(CT::CodePointOf(*iter)) };
			assert(ret <= 4);

			for (; Arithmetic(iter, iter, end, 1) & APRES::MOVED;)
			{
				ret += std::to_underlying(CT::CodePointOf(*iter));
			}

			return ret;
		}
	};

	inline constexpr auto as_multibytes = as_multibytes_t{};

	static_assert(IteratingPolicy<as_normal_ptr_t, char>);
	static_assert(IteratingPolicy<as_normal_ptr_t, wchar_t>);
	static_assert(IteratingPolicy<as_multibytes_t, unsigned char>);
	static_assert(IteratingPolicy<as_multibytes_t, wchar_t>);

	// Unit testing at: UnitTest_UtlString_IterPolicy.cpp
}

namespace Hydrogenium::StringPolicy::Comparing
{
	struct case_ignored_t final
	{
		struct detail final
		{
			template <typename T, typename U>
			__forceinline static constexpr void tolower_inplace(T& lhs, U& rhs) noexcept
			{
#ifndef HYDROGENIUM_UTL_UNICODE
				if (CType<T>::IsUpper(lhs))
					lhs = CType<T>::ToLower(lhs);
				if (CType<U>::IsUpper(rhs))
					rhs = CType<U>::ToLower(rhs);
#else
				lhs = CType<T>::ToLower(lhs);
				rhs = CType<U>::ToLower(rhs);
#endif
			}
		};

		template <typename T, typename U> [[nodiscard]]
		static constexpr int Cmp(T lhs, U rhs) noexcept
		{
			if constexpr (sizeof(T) == sizeof(U))
			{
				detail::tolower_inplace(lhs, rhs);
				return lhs - rhs;
			}
			else
			{
				auto c32_l = CType<T>::ToFullWidth(lhs);
				auto c32_r = CType<U>::ToFullWidth(rhs);

				detail::tolower_inplace(c32_l, c32_r);
				return c32_l - c32_r;
			}

			std::unreachable();
		}

		template <typename T, typename U> [[nodiscard]]
		static constexpr bool Eql(T lhs, U rhs) noexcept
		{
			if constexpr (sizeof(T) == sizeof(U))
			{
				detail::tolower_inplace(lhs, rhs);
				return lhs == rhs;
			}
			else
			{
				auto c32_l = CType<T>::ToFullWidth(lhs);
				auto c32_r = CType<U>::ToFullWidth(rhs);

				detail::tolower_inplace(c32_l, c32_r);
				return c32_l == c32_r;
			}

			std::unreachable();
		}
	};

	inline constexpr auto case_ignored = case_ignored_t{};

	struct regular_t final
	{
		[[nodiscard]]
		static constexpr int Cmp(auto lhs, auto rhs) noexcept
		{
			return lhs - rhs;
		}

		[[nodiscard]]
		static constexpr bool Eql(auto lhs, auto rhs) noexcept
		{
			return lhs == rhs;
		}
	};

	inline constexpr auto regular = regular_t{};

	static_assert(ComparingPolicy<case_ignored_t, char>);
	static_assert(ComparingPolicy<regular_t, char>);
}

namespace Hydrogenium::StringPolicy::Direction
{
	struct forwards_t final
	{
		inline static constexpr bool is_reverse = false;

		static constexpr auto Begin(auto&& r) noexcept
		{
			return std::ranges::begin(r);
		}

		static constexpr auto End(auto&& r) noexcept
		{
			return std::ranges::end(r);
		}
	};

	inline constexpr auto front_to_back = forwards_t{};

	struct backwards_t final
	{
		inline static constexpr bool is_reverse = true;

		static constexpr auto Begin(auto&& r) noexcept
		{
			return std::ranges::rbegin(r);
		}

		static constexpr auto End(auto&& r) noexcept
		{
			return std::ranges::rend(r);
		}
	};

	inline constexpr auto back_to_front = backwards_t{};

	static_assert(DirectionPolicy<StringPolicy::Direction::forwards_t, char>);
	static_assert(DirectionPolicy<StringPolicy::Direction::backwards_t, char>);
}

namespace Hydrogenium::StringPolicy::Result
{
	// Chr, SpnP(Spn), PBrk(CSpn), Str, Tok
	struct as_pointer_t final
	{
		[[nodiscard]]
		constexpr auto operator() (auto&& /*src*/, auto&& view, auto&& /*IterPolicy*/) const noexcept -> std::remove_cvref_t<decltype(view)>::const_pointer
		{
			if (view.empty())
				return nullptr;

			return std::addressof(view.front());
		}

		[[nodiscard]]
		constexpr auto operator() (auto&& /*abs_begin*/, auto&& abs_end, auto&& result_loc, auto&& /*IterPolicy*/) const noexcept
			-> decltype(std::addressof(*result_loc))
		{
			if (result_loc == abs_end)
				return nullptr;

			return std::addressof(*result_loc);
		}
	};
	struct as_position_t final
	{
		[[nodiscard]]
		constexpr auto operator() (auto&& src, auto&& view, auto IterPolicy) const noexcept -> std::ptrdiff_t
		{
			if (view.empty())	// return strcnt() equivalent.
			{
				std::ptrdiff_t counter = src.empty() ? 0 : 1;
				auto const begin = src.begin(), end = src.end();

				for (auto it = src.begin();
					IterPolicy.Arithmetic(it, begin, end, 1) & StringPolicy::Iterating::APRES::MOVED;
					++counter)
				{
					// nothing.
				}

				return counter;
			}

			auto const p1 = std::addressof(src.front());
			auto const p2 = std::addressof(view.front());

			if constexpr (IterPolicy.normal_pointer)
			{
				// it is undefined behaviour if view is not a subrange of src.
				return p2 - p1;
			}
			else
			{
				std::ptrdiff_t counter = 1;	// our pointer increment is in the testing phase, not post phase of for loop.
				std::remove_cvref_t<decltype(view)> const discard{ p1, p2 };// it is undefined behaviour if view is not a subrange of src.
				auto const begin = discard.begin(), end = discard.end();	// no range policy involved. the return value is always forwarding direction.

				for (auto it = discard.begin();
					IterPolicy.Arithmetic(it, begin, end, 1) & StringPolicy::Iterating::APRES::MOVED;
					++counter)
				{
					// nothing.
				}

				return counter;
			}
		}

		// abs_begin must be initialized by IterPolicy. Serve as point result.
		[[nodiscard]]
		constexpr auto operator() (auto&& abs_begin, auto&& abs_end, auto&& result_loc, auto&& IterPolicy) const noexcept
			-> std::ptrdiff_t
		{
			// cvref ignored.
			static_assert(typeid(abs_begin) == typeid(abs_end) && typeid(abs_end) == typeid(result_loc));

			using StringPolicy::Iterating::APRES;

			auto fwd_abs_begin = ToForwardIter(abs_begin, false);
			auto fwd_abs_end = ToForwardIter(abs_end, false);
			auto fwd_res_loc = ToForwardIter(result_loc, result_loc != abs_end);

			if (fwd_abs_begin > fwd_abs_end)	// begin should always less than end.
				std::swap(fwd_abs_begin, fwd_abs_end);

			if (result_loc == abs_end)	// no found!
				fwd_res_loc = fwd_abs_end;

			if constexpr (IterPolicy.normal_pointer)
			{
				// program is ill-formed if view_begin is not coming from abs_begin.
				return fwd_res_loc - fwd_abs_begin;
			}
			else
			{
				std::ptrdiff_t counter = fwd_abs_begin == fwd_res_loc ? 0 : 1;
				for (auto it = fwd_abs_begin;	// no range policy involved. the return value is always forwarding direction.
					IterPolicy.Arithmetic(it, fwd_abs_begin, fwd_res_loc, 1) & APRES::MOVED;
					++counter)
				{
					// nothing.
				}

				return counter;
			}
		}
	};
	struct as_view_t final
	{
		// For Tok() only
		[[nodiscard]]
		constexpr auto operator() (auto&& /*src*/, auto&& view, auto&& /*IterPolicy*/) const noexcept -> std::remove_cvref_t<decltype(view)>
		{
			return view;
		}

		// For the rest Query functors.
		// abs_begin must be initialized by IterPolicy. Return a view from result_loc to fwd_true_end
		[[nodiscard]]
		constexpr auto operator() (auto&& abs_begin, auto&& abs_end, auto&& result_loc, auto&& IterPolicy) const noexcept	// #MSVC_BUGGED_tailing_return_type_namespace_error
		{
			// cvref ignored.
			static_assert(typeid(abs_begin) == typeid(abs_end) && typeid(abs_end) == typeid(result_loc));

			using StringPolicy::Iterating::APRES;

			auto fwd_abs_begin = ToForwardIter(abs_begin, false);
			auto fwd_abs_end = ToForwardIter(abs_end, false);
			auto fwd_res_loc = ToForwardIter(result_loc, result_loc != abs_end);

			if (fwd_abs_begin > fwd_abs_end)	// begin should always less than end.
				std::swap(fwd_abs_begin, fwd_abs_end);

			if (result_loc == abs_end)	// no found!
				fwd_res_loc = fwd_abs_end;

			return typename CType<decltype(*result_loc)>::view_type{ fwd_res_loc, fwd_abs_end };
		}
	};

	// Cmp
	struct as_lexic_t final
	{
		[[nodiscard]]
		constexpr int operator() (std::same_as<int> auto val) const noexcept
		{
			return val;
		}
	};
	struct as_stl_ordering_t final
	{
		[[nodiscard]]
		constexpr std::strong_ordering operator() (std::same_as<int> auto val) const noexcept
		{
			if (val < 0)
				return std::strong_ordering::less;
			else if (val > 0)
				return std::strong_ordering::greater;
			else
				return std::strong_ordering::equal;	// that's what expression 0<=>0 will return.
		}
	};
	struct as_eql_t final { [[nodiscard]] constexpr bool operator()(std::same_as<int> auto val) const noexcept { return val == 0; } };
	struct as_lt_t final { [[nodiscard]] constexpr bool operator()(std::same_as<int> auto val) const noexcept { return val < 0; } };
	struct as_lt_eq_t final { [[nodiscard]] constexpr bool operator()(std::same_as<int> auto val) const noexcept { return val <= 0; } };
	struct as_gt_t final { [[nodiscard]] constexpr bool operator()(std::same_as<int> auto val) const noexcept { return val > 0; } };
	struct as_gt_eq_t final { [[nodiscard]] constexpr bool operator()(std::same_as<int> auto val) const noexcept { return val >= 0; } };
	struct as_not_eq_t final { [[nodiscard]] constexpr bool operator()(std::same_as<int> auto val) const noexcept { return val != 0; } };

	// Cnt(Len) #UPDATE_AT_CPP26 sat_cast
	struct as_signed_t final
	{
		[[nodiscard]]
		constexpr auto operator() (std::same_as<size_t> auto n) const noexcept -> std::make_signed_t<size_t>
		{
			n = std::min(n, (size_t)std::numeric_limits<std::make_signed_t<size_t>>::max());
			return static_cast<std::make_signed_t<size_t>>(n);
		}
	};
	struct as_unsigned_t final
	{
		[[nodiscard]]
		constexpr size_t operator() (std::same_as<size_t> auto n) const noexcept
		{
			return n;
		}
	};

	// Dup(Rev), Lwr, Upr
	struct as_unmanaged_t final
	{
		[[nodiscard]]
		constexpr auto operator()(auto&& view) const noexcept -> std::remove_cvref_t<decltype(view)>::pointer
		{
			if (view.empty())
				return nullptr;

			auto p = new std::remove_cvref_t<decltype(view)>::value_type[view.length() + 1]{};

			for (size_t i = 0; i < view.size(); ++i)
				p[i] = view[i];

			return p;
		}

		template <typename proj_t = std::identity> [[nodiscard]]
		constexpr auto operator()(auto&& first, auto&& last, auto IterPolicy, proj_t proj = {}) const noexcept
		{
			using char_type = std::remove_cvref_t<decltype(*first)>;
			using value_type = decltype(IterPolicy.ValueOf(first));

			auto p = new char_type[IterPolicy.NativeSize(first, last) + 1]{};
			auto i = 0;

			// remove cvref comparison
			static_assert(typeid(std::invoke_result_t<proj_t, value_type>) == typeid(value_type));

			for (auto it = first; it < last; IterPolicy.Arithmetic(it, first, last, 1))
			{
				if constexpr (sizeof(char_type) == sizeof(value_type))
				{
					p[i++] = proj(IterPolicy.ValueOf(it));
				}

				// bad, very bad.
				else
				{
					static_assert(typeid(fchar_t) == typeid(value_type));

					auto const [bytes, count] = CType<char_type>::ToMultiBytes(proj(IterPolicy.ValueOf(it)));
					assert(count > 0);

					for (auto&& byt : bytes | std::views::take(count))
					{
						p[i++] = byt;
					}
				}
			}

			return p;
		}
	};
	struct as_marshaled_t final
	{
		[[nodiscard]]
		constexpr auto operator()(auto&& view) const noexcept // #MSVC_BUGGED_tailing_return_type_namespace_error
		{
			return std::basic_string<typename std::remove_cvref_t<decltype(view)>::value_type>{
				std::forward<decltype(view)>(view)
			};
		}

		template <typename proj_t = std::identity> [[nodiscard]]
		constexpr auto operator()(auto&& first, auto&& last, auto IterPolicy, proj_t proj = {}) const noexcept
		{
			using char_type = std::remove_cvref_t<decltype(*first)>;
			using value_type = decltype(IterPolicy.ValueOf(first));

			std::basic_string<char_type> ret{};
			ret.reserve(IterPolicy.NativeSize(first, last));

			// remove cvref comparison
			static_assert(typeid(std::invoke_result_t<proj_t, value_type>) == typeid(value_type));

			for (auto it = first; it < last; IterPolicy.Arithmetic(it, first, last, 1))
			{
				if constexpr (sizeof(char_type) == sizeof(value_type))
				{
					ret.push_back(proj(IterPolicy.ValueOf(it)));
				}

				// bad, very bad.
				else
				{
					static_assert(typeid(fchar_t) == typeid(value_type));

					auto const [bytes, count] = CType<char_type>::ToMultiBytes(proj(IterPolicy.ValueOf(it)));
					assert(count > 0);

					ret.append_range(
						bytes | std::views::take(count)
					);
				}
			}

			return ret;
		}
	};

	// Tok
	struct as_generator_t final {};	// #UPDATE_AT_CPP23 generator
	struct as_vector_t final {};
	inline constexpr auto as_generator = as_generator_t{};
	inline constexpr auto as_vector = as_vector_t{};

	template <typename Q, typename T, typename C, typename M>
	struct postprocessor_t final
	{
		inline static constexpr Q Query = Q{};
		inline static constexpr T Test = T{};
		inline static constexpr C Counting = C{};
		inline static constexpr M Modify = M{};
	};

	inline constexpr auto as_it_is = postprocessor_t<as_view_t, as_lexic_t, as_signed_t, as_marshaled_t>{};
	static_assert(ResultProcessor<decltype(as_it_is), char>);

	inline constexpr auto to_c_style = postprocessor_t<as_pointer_t, as_lexic_t, as_unsigned_t, as_unmanaged_t>{};
	static_assert(ResultProcessor<decltype(to_c_style), char>);
}

namespace Hydrogenium::String::Functors::Components
{
	struct base_comp_t
	{
		// Only for non-dependented name, quick diagnosis.
		// https://en.cppreference.com/w/cpp/language/dependent_name
		template <typename CFinal>
		static consteval bool VerifyComponents()
		{
			static_assert(requires{ typename CFinal::policy_iter; }, "Requires an iterator policy component.");
			static_assert(requires{ typename CFinal::policy_cmp; }, "Requires an comparator policy component.");
			static_assert(requires{ typename CFinal::invk_sig; }, "Requires an invoking policy component.");
			static_assert(requires{ typename CFinal::policy_dir; }, "Requires an searching direction policy component.");
			static_assert(requires{ typename CFinal::policy_ret; }, "Requires an return transforming policy component.");

			return true;
		}

		// Just keep compiler happy.
		void operator()(...) const noexcept = delete;
	};

	template <typename CFinal, template <typename, typename> class TFirst, template <typename, typename> class... TRests>
	struct Linker : TFirst<CFinal, Linker<CFinal, TRests...>> { using TFirst<CFinal, Linker<CFinal, TRests...>>::operator(); };

	template <typename CFinal, template <typename, typename> class TFirst>
	struct Linker<CFinal, TFirst> : TFirst<CFinal, base_comp_t> { using TFirst<CFinal, base_comp_t>::operator(); };

	// Iterating

	template <typename CFinal, typename Base>
	struct iter_multibytes : Base
	{
		using policy_iter = StringPolicy::Iterating::as_multibytes_t;
		static_assert(!requires{ typename Base::policy_iter; }, "Only one iterator policy allowed!");
	};

	template <typename CFinal, typename Base>
	struct iter_default : Base
	{
		using policy_iter = StringPolicy::Iterating::as_normal_ptr_t;
		static_assert(!requires{ typename Base::policy_iter; }, "Only one iterator policy allowed!");
	};

	// Comparing

	template <typename CFinal, typename Base>
	struct cmp_case_ignored : Base
	{
		using policy_cmp = StringPolicy::Comparing::case_ignored_t;
		static_assert(!requires{ typename Base::policy_cmp; }, "Only one comparing policy allowed!");
	};

	template <typename CFinal, typename Base>
	struct cmp_default : Base
	{
		using policy_cmp = StringPolicy::Comparing::regular_t;
		static_assert(!requires{ typename Base::policy_cmp; }, "Only one comparing policy allowed!");
	};

	// Direction

	template <typename CFinal, typename Base>
	struct dir_forward : Base
	{
		using policy_dir = StringPolicy::Direction::forwards_t;
		static_assert(!requires{ typename Base::policy_dir; }, "Only one directional policy allowed!");
	};

	template <typename CFinal, typename Base>
	struct dir_backward : Base
	{
		using policy_dir = StringPolicy::Direction::backwards_t;
		static_assert(!requires{ typename Base::policy_dir; }, "Only one directional policy allowed!");
	};

	// Result #CONTINUE_FROM_HERE

	template <typename CIncompleteType, typename Base>
	struct ret_as_it_is : Base
	{
		using policy_ret = ret_as_it_is;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		// Chr, SpnP(Spn), PBrk(CSpn), Str, Tok
		template <typename CFinal = CIncompleteType>
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(CFinal::iter_type const& /*abs_begin*/, CFinal::iter_type const& /*abs_end*/, CFinal::iter_type const& result_loc) noexcept
		{
			return result_loc;
		}

		// Cmp, Cnt(Len), Tok
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(auto&& arg0) noexcept
		{
			return std::move(arg0);
		}

		// Dup(Rev), Lwr, Upr => Unsupported.
	};

	// Chr, SpnP(Spn), PBrk(CSpn), Str

	template <typename CIncompleteType, typename Base>
	struct ret_as_pointer : Base
	{
		using policy_ret = ret_as_pointer;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		template <typename CFinal = CIncompleteType>
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(CFinal::iter_type const& abs_begin, CFinal::iter_type const& abs_end, CFinal::iter_type const& result_loc) noexcept
		{
			return StringPolicy::Result::as_pointer_t{}(abs_begin, abs_end, result_loc);
		}
	};

	template <typename CIncompleteType, typename Base>
	struct ret_as_abs_pos : Base
	{
		using policy_ret = ret_as_abs_pos;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		template <typename CFinal = CIncompleteType>
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(CFinal::iter_type const& abs_begin, CFinal::iter_type const& abs_end, CFinal::iter_type const& result_loc) noexcept
		{
			return StringPolicy::Result::as_position_t{}(abs_begin, abs_end, result_loc);
		}
	};

	template <typename CIncompleteType, typename Base>
	struct ret_as_rel_pos : Base
	{
		using policy_ret = ret_as_rel_pos;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		template <typename CFinal = CIncompleteType>
		[[nodiscard]]
		static constexpr decltype(auto) Transform(CFinal::iter_type const& abs_begin, CFinal::iter_type const& abs_end, CFinal::iter_type const& result_loc) noexcept
		{
			using StringPolicy::Iterating::APRES;

			if constexpr (CFinal::policy_iter::normal_pointer)
			{
				// program is ill-formed if view_begin is not coming from abs_begin.
				return result_loc - abs_begin;
			}
			else
			{
				std::ptrdiff_t counter{};
				auto const DiscardView = BuildStringView(abs_begin, result_loc, abs_end, false);
				auto [begin, it, end]
					= CFinal::policy_iter::Get(DiscardView, typename CFinal::policy_dir{}, std::numeric_limits<ptrdiff_t>::max());

				for (; it < end; ++counter, CFinal::policy_iter::Arithmetic(it, begin, end, 1))
				{
					// nothing.
				}

				return counter;
			}
		}
	};

	template <typename CIncompleteType, typename Base>
	struct ret_as_view : Base
	{
		using policy_ret = ret_as_view;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		template <typename CFinal = CIncompleteType>
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(CFinal::iter_type const& abs_begin, CFinal::iter_type const& abs_end, CFinal::iter_type const& result_loc) noexcept
		{
			return StringPolicy::Result::as_view_t{}(abs_begin, abs_end, result_loc);
		}
	};

	// Cmp

	template <typename CIncompleteType, typename Base>
	struct ret_as_stl_ordering : Base
	{
		using policy_ret = ret_as_stl_ordering;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(std::same_as<int> auto val) noexcept
		{
			return StringPolicy::Result::as_stl_ordering_t{}(val);
		}
	};

	// Cnt(Len) #UPDATE_AT_CPP26 sat_cast

	template <typename CIncompleteType, typename Base>
	struct ret_as_signed : Base
	{
		using policy_ret = ret_as_signed;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(std::same_as<size_t> auto val) noexcept
		{
			return StringPolicy::Result::as_signed_t{}(val);
		}
	};

	// Dup(Rev), Lwr, Upr

	template <typename CIncompleteType, typename Base>
	struct ret_as_unmanaged : Base
	{
		using policy_ret = ret_as_unmanaged;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		// Keep this two as auto&&
		// The input can be iter of std::string

		template <typename proj_t = std::identity, typename CFinal = CIncompleteType>
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(auto&& first, auto&& last, proj_t proj = {}) noexcept
			requires(typeid(*first) == typeid(*last) && typeid(*last) == typeid(CFinal::char_type))
		{
			return StringPolicy::Result::as_unmanaged_t{}(first, last, typename CFinal::policy_iter{}, proj);
		}
	};

	template <typename CIncompleteType, typename Base>
	struct ret_as_marshaled : Base
	{
		using policy_ret = ret_as_marshaled;
		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transform policy allowed!");

		// Keep this two as auto&&
		// The input can be iter of std::string

		template <typename proj_t = std::identity, typename CFinal = CIncompleteType>
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(auto&& first, auto&& last, proj_t proj = {}) noexcept
			requires(typeid(*first) == typeid(*last) && typeid(*last) == typeid(CFinal::char_type))
		{
			return StringPolicy::Result::as_marshaled_t{}(first, last, typename CFinal::policy_iter{}, proj);
		}
	};

	// Tok

	// I don't think it is doable.
	// It's of lazy and active difference.
}

namespace Hydrogenium::String
{
	template <
		typename char_type = char,
		StringPolicy::IteratingPolicy<char_type> auto IterPolicy = StringPolicy::Iterating::as_regular_ptr,
		StringPolicy::ComparingPolicy<char_type> auto Comparator = StringPolicy::Comparing::regular,
		StringPolicy::DirectionPolicy<char_type> auto RangePolicy = StringPolicy::Direction::front_to_back,
		StringPolicy::ResultProcessor<char_type> auto RsltProc = StringPolicy::Result::as_it_is
	>
	struct Utils final
	{
		using ctype_info = CType<char_type>;
		using nullable_type = std::optional<typename ctype_info::view_type>;
		using owner_type = ctype_info::owner_type;
		using owner_iter = decltype(RangePolicy.Begin(owner_type{}));
		using param_type = ctype_info::param_type;
		using value_type = decltype(IterPolicy.ValueOf(owner_iter{}));
		using view_type = ctype_info::view_type;
		using view_iter = decltype(RangePolicy.Begin(view_type{}));

		struct detail final
		{
			static inline constexpr auto default_search_len = std::numeric_limits<ptrdiff_t>::max();

			// Chr - v, c -> string_view; No internal impl needed.
			// Cmp - v, v -> int; No internal impl needed.

			// Cnt - v -> size_t; Counting graphemes in a char[] range.
			__forceinline static constexpr size_t Cnt(view_type const& str, ptrdiff_t count = default_search_len) noexcept
			{
				auto [begin, it, end] = IterPolicy.Get(str, RangePolicy, count);

				size_t n = begin == end ? 0 : 1;	// #UPDATE_AT_CPP23 size type literal
				for (; IterPolicy.Arithmetic(it, begin, end, 1) & StringPolicy::Iterating::APRES::MOVED; ++n) {}

				return n;
			}

			// CSpn - v, v -> size_t; Use PBrk instead. This one will count distance from RangePolicy.Begin() instead of from absolute start. 'R' stands for 'relative'.
			static constexpr ptrdiff_t CSpnR(view_type const& dest, view_type const& src, ptrdiff_t count = default_search_len, bool bSpnMode = false) noexcept
			{
				// the src is NOT order-sensitive, it's no more than a set of what to search.
				// hence no range function put onto src.

				auto [b1, s1, e1] = IterPolicy.Get(dest, RangePolicy, count);
				auto [b2, s2, e2] = IterPolicy.Get(src, StringPolicy::Direction::front_to_back, default_search_len);
				ptrdiff_t counter = 0;

				for (; s1 < e1; IterPolicy.Arithmetic(s1, b1, e1, 1), ++counter)
				{
					bool found_in_src = false;
					auto const ch1 = IterPolicy.ValueOf(s1);

					for (s2 = b2; s2 < e2; IterPolicy.Arithmetic(s2, b2, e2, 1))
					{
						auto const ch2 = IterPolicy.ValueOf(s2);

						if (Comparator.Eql(ch1, ch2))
						{
							found_in_src = true;
							break;
						}
					}

					if (found_in_src == !bSpnMode)
						break;
				}

				return counter;
			}

			// Dup - v -> string; No interal impl needed. This one is for getting a view of first N graphemes.
			static constexpr auto DupV(view_type const& str, ptrdiff_t count) noexcept -> ctype_info::view_type
			{
				using StringPolicy::Iterating::APRES;

				// the moving iter is already included in this function. No additional loop needed!
				auto [begin, it, end] = IterPolicy.Get(str, RangePolicy, count);

				if constexpr (!RangePolicy.is_reverse)
				{
					return { begin, end };	// in the case of multibyte: begin always pointing to the head of UTF stream, if not reversed.
				}
				else
				{
					auto const fwit1 = ToForwardIter(end, false);	// we are taking last N characters, so including the 'ending' one just defeat the purpose.
					auto const fwed1 = ToForwardIter(begin, false);	// in reverse_iter, rbegin is the actual end.

					return { fwit1, fwed1 };
				}
			}

			// Fry - o -> string; WTF???? WHO NEEDS THIS? WHAT'S WRONG WITH YOU?
			static auto Fry(owner_type* psz, ptrdiff_t until = default_search_len) noexcept -> decltype(psz)
			{
				using int_type = std::conditional_t<sizeof(char_type) == 1, int8_t, std::conditional_t<sizeof(char_type) == 2, int16_t, int32_t>>;
				constexpr auto UTF_MAX = std::min<int32_t>(std::numeric_limits<int_type>::max(), 0x10FFFF);

				thread_local static std::random_device PureRD;
				thread_local static std::mt19937 gen{ PureRD() };
				thread_local static std::uniform_int_distribution<int32_t> distrb(1, UTF_MAX);	// avoid '\0' causing C bug.

				// we are just filling random garbage, the order doesn't matter.
				for (auto& ch : *psz | std::views::take(until))
				{
					ch = distrb(gen);
				}

				return psz;
			}

			// Len - v -> size_t; Use Cnt instead.

			// Lwr - o -> string
			__forceinline static constexpr auto Lwr(view_type const& str, ptrdiff_t count) noexcept
			{
				auto [_, it, end] = IterPolicy.Get(str, RangePolicy, count);

				constexpr auto to_lower =
					[](decltype(IterPolicy.ValueOf(it)) ch) noexcept -> decltype(ch)
					{
						return CType<decltype(ch)>::ToLower(ch);
					};

				return std::make_tuple(std::move(it), std::move(end), to_lower);
			}

			// PBrk - v, v -> string_view; served as CSpn, Spn, SpnP as well.
			__forceinline static constexpr auto PBrk(view_type const& dest, view_type const& src, ptrdiff_t count = default_search_len, bool bSpnPMode = false) noexcept
			{
				// the src is NOT order-sensitive, it's no more than a set of what to search.
				// hence no range function put onto src.

				auto [b1, s1, e1] = IterPolicy.Get(dest, RangePolicy, count);
				auto [b2, s2, e2] = IterPolicy.Get(src, StringPolicy::Direction::front_to_back, default_search_len);

				for (; s1 < e1; IterPolicy.Arithmetic(s1, b1, e1, 1))
				{
					bool found_in_src = false;
					auto const ch1 = IterPolicy.ValueOf(s1);

					for (s2 = b2; s2 < e2; IterPolicy.Arithmetic(s2, b2, e2, 1))
					{
						auto const ch2 = IterPolicy.ValueOf(s2);

						if (Comparator.Eql(ch1, ch2))
						{
							found_in_src = true;
							break;
						}
					}

					if (found_in_src == !bSpnPMode)
						break;
				}

				// why can't I use goto in constexpr?!
			//LAB_POST_FINDING:;

				return std::make_tuple(std::move(b1), std::move(e1), std::move(s1));
			}

			// Rev - o -> string; Use Dup instead.
			// Sep - o, v -> string; Use Tok instead.
			// Spn - v, v -> size_t; Use PBrk instead.
			// SpnP - v, v -> string_view; Use PBrk instead.
			// Str - v, v -> string_view; No internal impl needed.

			// Tok - n, v -> string_view; The second argument filled in will be altered!
			__forceinline static constexpr auto Tok(auto&& begin, auto& it, auto&& end, view_type const& delim) noexcept
				-> decltype(BuildStringView(begin, it, end, false))
			{
				// I. Move pointer to the next non-delim position.
				auto const org_before_comp = CSpnR(BuildStringView(it, end, end, false), delim, default_search_len, true);
				IterPolicy.Arithmetic(it, begin, end, org_before_comp);

				if (it >= end)
					return BuildStringView(end, end, end, false);

				// II. Move pointer to next delim position. And now 'it' serves as end.
				auto const tokenBegin = it;
				auto const org_before_comp2 = CSpnR(BuildStringView(it, end, end, false), delim);
				IterPolicy.Arithmetic(it, begin, end, org_before_comp2);

				// buffer is the end in this section as it is going to be assigned as '\0' in original strtok().
				return BuildStringView(tokenBegin, it, end, false);
			}

			// Upr - o -> string; For some reason compiler will complain about 'accessing released memory' if not convert to it's view form.
			__forceinline static constexpr auto Upr(view_type const& str, ptrdiff_t count) noexcept
			{
				auto [_, it, end] = IterPolicy.Get(str, RangePolicy, count);

				constexpr auto to_upper =
					[](decltype(IterPolicy.ValueOf(it)) ch) noexcept -> decltype(ch)
					{
						return CType<decltype(ch)>::ToUpper(ch);
					};

				return std::make_tuple(std::move(it), std::move(end), to_upper);
			}
		};

#pragma region Chr
		struct chr_fn_t
		{
			constexpr auto operator()(view_type const& str, param_type ch, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Query(RangePolicy.Begin(str), RangePolicy.End(str), RangePolicy.Begin(str), IterPolicy))
			{
				auto [begin, it, end] = IterPolicy.Get(str, RangePolicy, until);

				for (; it < end; IterPolicy.Arithmetic(it, begin, end, 1))
				{
					if (Comparator.Eql(IterPolicy.ValueOf(it), ch))
						break;
				}

				return RsltProc.Query(begin, end, it, IterPolicy);
			}
		};
		static inline constexpr auto Chr = chr_fn_t{};
#pragma endregion Chr

#pragma region Cmp
		struct cmp_fn_t
		{
			constexpr auto operator()(view_type const& lhs, view_type const& rhs, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Test(0))
			{
				auto [b1, s1, e1] = IterPolicy.Get(lhs, RangePolicy, count);
				auto [b2, s2, e2] = IterPolicy.Get(rhs, RangePolicy, count);

				while (
					s1 < e1 && s2 < e2
					&& Comparator.Eql(IterPolicy.ValueOf(s1), IterPolicy.ValueOf(s2))
					)
				{
					IterPolicy.Arithmetic(s1, b1, e1, 1);
					IterPolicy.Arithmetic(s2, b2, e2, 1);
				}

				// Preventing deducing as something like 'int32_t'
				value_type const c1 = s1 == e1 ? '\0' : IterPolicy.ValueOf(s1);
				value_type const c2 = s2 == e2 ? '\0' : IterPolicy.ValueOf(s2);

				return RsltProc.Test(
					Comparator.Cmp(c1, c2)
				);
			}
		};
		static inline constexpr auto Cmp = cmp_fn_t{};
#pragma endregion Cmp

#pragma region Cnt
		struct cnt_fn_t
		{
			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Counting(size_t{}))
			{
				return RsltProc.Counting(
					detail::Cnt(str, count)
				);
			}
		};
		static inline constexpr auto Cnt = cnt_fn_t{};
#pragma endregion Cnt

#pragma region Dup
		struct dup_fn_t
		{
			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Modify(RangePolicy.Begin(str), RangePolicy.End(str), IterPolicy))
			{
				auto [_, it, logical_end] = IterPolicy.Get(str, RangePolicy, count);

				// the count param is for size in the native type. Not count of graphemes.
				return RsltProc.Modify(it, logical_end, IterPolicy);
			}
		};
		static inline constexpr auto Dup = dup_fn_t{};
#pragma endregion Dup

#pragma region Lwr
		struct lwr_fn_t
		{
			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
			{
			auto [_, it, end] = IterPolicy.Get(str, RangePolicy, count);

				return RsltProc.Modify(it, end, IterPolicy, &CType<value_type>::ToLower);
			}

			// return type is void, in the case of in_place mode.
			constexpr void operator()(owner_type* pstr, ptrdiff_t count = detail::default_search_len) const noexcept
			{
				// For some reason, it must look like this can the compiler happy about 'accessing released memory'.

				auto [it, end, functor] = detail::Lwr(*pstr, count);

				static_assert(
					typeid(RsltProc.Modify(it, end, IterPolicy, functor)) == typeid(owner_type),
					"Lwr() method must be used with marshaled returning types."
				);
				*pstr = RsltProc.Modify(it, end, IterPolicy, functor);
			}
		};
		static inline constexpr auto Lwr = lwr_fn_t{};
#pragma endregion Lwr

#pragma region PBrk
		struct pbrk_fn_t
		{
			constexpr auto operator()(view_type const& dest, view_type const& src, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Query(dest, src, IterPolicy))
			{
				auto [begin, end, it] = detail::PBrk(dest, src, count, false);

				return RsltProc.Query(
					begin, end, it,
					IterPolicy
				);
			}
		};
		static inline constexpr auto PBrk = pbrk_fn_t{};
#pragma endregion PBrk

#pragma region SpnP
		struct spnp_fn_t
		{
			constexpr auto operator()(view_type const& dest, view_type const& src, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Query(dest, src, IterPolicy))
			{
				auto [begin, end, it] = detail::PBrk(dest, src, count, true);

				return RsltProc.Query(
					begin, end, it,
					IterPolicy
				);
			}
		};
		static inline constexpr auto SpnP = spnp_fn_t{};
#pragma endregion SpnP

#pragma region Str
		struct str_fn_t
		{
			constexpr auto operator()(view_type const& str, view_type const& substr, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Query(RangePolicy.Begin(str), RangePolicy.End(str), RangePolicy.Begin(str), IterPolicy))
			{
				// Searching direction is nothing to do with comparing direction!!
				// the substr is going to attempting to match with the forwarding order.
				// hence no range function put onto src.

				auto [b1, s1, e1] = IterPolicy.Get(str, RangePolicy, until);
				auto const iSubstrCnt = detail::Cnt(substr);

				constexpr auto FwdCmp = Utils<
					char_type,
					IterPolicy,
					Comparator,
					StringPolicy::Direction::front_to_back,
					StringPolicy::Result::as_it_is	// To guarantee a lexicographical result.
				>::Cmp;

				if constexpr (!RangePolicy.is_reverse)
				{
					for (; s1 < e1; IterPolicy.Arithmetic(s1, b1, e1, 1))
					{
						if (FwdCmp({ s1, e1 }, substr, iSubstrCnt) == 0)
							break;
					}
				}
				else
				{
					for (; s1 < e1; IterPolicy.Arithmetic(s1, b1, e1, 1))
					{
						auto const fwit1 = ToForwardIter(s1);
						auto const fwed1 = ToForwardIter(b1, false);	// in reverse_iter, rbegin is the actual end.

						if (FwdCmp({ fwit1, fwed1 }, substr, iSubstrCnt) == 0)
							break;
					}
				}

				return RsltProc.Query(b1, e1, s1, IterPolicy);
			}
		};
		static inline constexpr auto Str = str_fn_t{};
#pragma endregion Str

#pragma region Tok
		struct tok_fn_t
		{
			auto operator()(nullable_type const& str, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(RsltProc.Query(*str, delim, IterPolicy))
			{
				static thread_local std::tuple_element_t<0, decltype(IterPolicy.Get(*str, RangePolicy, until))> begin{};
				static thread_local std::tuple_element_t<1, decltype(IterPolicy.Get(*str, RangePolicy, until))> it{};
				static thread_local std::tuple_element_t<2, decltype(IterPolicy.Get(*str, RangePolicy, until))> end{};

				if (str.has_value())
					std::tie(begin, it, end) = IterPolicy.Get(*str, RangePolicy, until);	// in all other calling case, 'until' param will be ignored.

				return RsltProc.Query(
					str,
					detail::Tok(begin, it, end, delim),
					IterPolicy
				);
			}

			auto operator()(StringPolicy::Result::as_generator_t, view_type const& str, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
				-> GENERATOR_TY<view_type>
			{
				auto [begin, it, end] = IterPolicy.Get(str, RangePolicy, until);

				for (auto view = detail::Tok(begin, it, end, delim); !view.empty(); view = detail::Tok(begin, it, end, delim))
				{
					co_yield view;
				}

				co_return;
			}

			constexpr auto operator()(StringPolicy::Result::as_vector_t, view_type const& str, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
				-> std::vector<view_type>
			{
				auto [begin, it, end] = IterPolicy.Get(str, RangePolicy, until);
				std::vector<view_type> ret{};

				for (auto view = detail::Tok(begin, it, end, delim); !view.empty(); view = detail::Tok(begin, it, end, delim))
				{
					ret.emplace_back(std::move(view));
				}

				return ret;
			}
		};
		static inline constexpr auto Tok = tok_fn_t{};
#pragma endregion Tok

#pragma region Upr
		struct upr_fn_t
		{
			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
			{
				auto [_, it, end] = IterPolicy.Get(str, RangePolicy, count);

				return RsltProc.Modify(it, end, IterPolicy, &CType<value_type>::ToUpper);
			}

			// return type is void, in the case of in_place mode.
			constexpr void operator()(owner_type* pstr, ptrdiff_t count = detail::default_search_len) const noexcept
			{
				// For some reason, it must look like this can the compiler happy about 'accessing released memory'.

				auto [it, end, functor] = detail::Upr(*pstr, count);

				static_assert(
					typeid(RsltProc.Modify(it, end, IterPolicy, functor)) == typeid(typename ctype_info::owner_type),
					"Upr() method must be used with marshaled returning types."
				);
				*pstr = RsltProc.Modify(it, end, IterPolicy, functor);
			}
		};
		static inline constexpr auto Upr = upr_fn_t{};
#pragma endregion Upr
	};
}

namespace Hydrogenium
{
	namespace detail::strutl_decl
	{
		using namespace String;
		using namespace StringPolicy;

		using Str	= Utils<>;
		using StrI	= Utils<char,		Iterating::as_regular_ptr,	Comparing::case_ignored>;
		using StrR	= Utils<char,		Iterating::as_regular_ptr,	Comparing::regular,			Direction::back_to_front>;
		using StrIR	= Utils<char,		Iterating::as_regular_ptr,	Comparing::case_ignored,	Direction::back_to_front>;

		using Wcs	= Utils<wchar_t>;
		using WcsI	= Utils<wchar_t,	Iterating::as_regular_ptr,	Comparing::case_ignored>;
		using WcsR	= Utils<wchar_t,	Iterating::as_regular_ptr,	Comparing::regular,			Direction::back_to_front>;
		using WcsIR	= Utils<wchar_t,	Iterating::as_regular_ptr,	Comparing::case_ignored,	Direction::back_to_front>;

		using Mbs	= Utils<char,		Iterating::as_multibytes>;
		using MbsI	= Utils<char,		Iterating::as_multibytes,	Comparing::case_ignored>;
		using MbsR	= Utils<char,		Iterating::as_multibytes,	Comparing::regular,			Direction::back_to_front>;
		using MbsIR	= Utils<char,		Iterating::as_multibytes,	Comparing::case_ignored,	Direction::back_to_front>;

		inline constexpr auto MbsCSpn = Utils<
			char,
			Iterating::as_multibytes,
			Comparing::regular,
			Direction::front_to_back,
			Result::postprocessor_t<Result::as_position_t, Result::as_lexic_t, Result::as_unsigned_t, Result::as_unmanaged_t>{}
		> ::PBrk;

		inline constexpr auto MbsSpn = Utils<
			char,
			Iterating::as_multibytes,
			Comparing::regular,
			Direction::front_to_back,
			Result::postprocessor_t<Result::as_position_t, Result::as_lexic_t, Result::as_unsigned_t, Result::as_unmanaged_t>{}
		> ::SpnP;

		inline constexpr auto MbsRCSpn = Utils<
			char,
			Iterating::as_multibytes,
			Comparing::regular,
			Direction::back_to_front,
			Result::postprocessor_t<Result::as_position_t, Result::as_lexic_t, Result::as_unsigned_t, Result::as_unmanaged_t>{}
		> ::PBrk;

		inline constexpr auto MbsRSpn = Utils<
			char,
			Iterating::as_multibytes,
			Comparing::regular,
			Direction::back_to_front,
			Result::postprocessor_t<Result::as_position_t, Result::as_lexic_t, Result::as_unsigned_t, Result::as_unmanaged_t>{}
		> ::SpnP;
	}

	using detail::strutl_decl::Str;
	using detail::strutl_decl::StrI;
	using detail::strutl_decl::StrR;
	using detail::strutl_decl::StrIR;

	using detail::strutl_decl::Wcs;
	using detail::strutl_decl::WcsI;
	using detail::strutl_decl::WcsR;
	using detail::strutl_decl::WcsIR;

	using detail::strutl_decl::Mbs;
	using detail::strutl_decl::MbsI;
	using detail::strutl_decl::MbsR;
	using detail::strutl_decl::MbsIR;

	using detail::strutl_decl::MbsCSpn;
	using detail::strutl_decl::MbsSpn;
	using detail::strutl_decl::MbsRCSpn;
	using detail::strutl_decl::MbsRSpn;
}

#ifdef GENERATOR_TY
#undef GENERATOR_TY
#endif



constexpr auto UTIL_Trim(std::string_view sv) noexcept -> decltype(sv)
{
	using namespace Hydrogenium;

	auto ret =
		sv
		| std::views::drop_while(CType<char>::IsSpace)
		| std::views::reverse
		| std::views::drop_while(CType<char>::IsSpace)
		| std::views::reverse;

	if (std::ranges::empty(ret))
		return "";

	return {
		ret.begin().base().base(),
		ret.end().base().base(),
	};
}

static_assert(UTIL_Trim("").empty());
static_assert(UTIL_Trim(" \r\n\t").empty());
static_assert(UTIL_Trim(" abc ") == "abc");
static_assert(UTIL_Trim(" abc") == "abc");
static_assert(UTIL_Trim("abc ") == "abc");
static_assert(UTIL_Trim("abc") == "abc");

// UTIL_ReplaceAll
// UTIL_Split