/*
	Created at: May 10 2024
*/

#pragma once

#define HYDROGENIUM_UTL_STRING 20240520L

#include <cassert>
#include <cctype>
#include <cstdint>
#include <cwctype>

#include <algorithm>
#include <bit>		// u8buffer, bit_cast
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

// Misc, Part I
namespace Hydrogenium
{
	template <typename T>
	concept NonVoid = !std::is_same_v<T, void>;

	template <typename T, typename C>
	concept IteratorOf = requires (T iter)
	{
		{ *iter } -> std::convertible_to<C>;
		requires std::bidirectional_iterator<std::remove_cvref_t<T>>;

		//std::is_same_v<std::remove_cvref_t<T>, std::add_pointer_t<std::remove_cvref_t<C>>>
		//|| std::is_same_v<std::remove_cvref_t<T>, std::add_pointer_t<std::add_const_t<std::remove_cvref_t<C>>>>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string<std::remove_cvref_t<C>>::iterator>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string<std::remove_cvref_t<C>>::const_iterator>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string<std::remove_cvref_t<C>>::reverse_iterator>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string<std::remove_cvref_t<C>>::const_reverse_iterator>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string_view<std::remove_cvref_t<C>>::iterator>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string_view<std::remove_cvref_t<C>>::const_iterator>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string_view<std::remove_cvref_t<C>>::reverse_iterator>
		//|| std::is_same_v<std::remove_cvref_t<T>, typename std::basic_string_view<std::remove_cvref_t<C>>::const_reverse_iterator>;
	};

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

// u8 and u16buffer
namespace Hydrogenium
{
	template <typename C>
	struct multibytes_t final
	{
		static_assert(sizeof(C) < 4, "Use with char8_t or char16_t!");
		static inline constexpr auto MAX_SIZE = sizeof(uint32_t) / sizeof(C);
		static inline constexpr auto BACK = MAX_SIZE - 1;

		[[nodiscard]]
		constexpr auto begin(this auto&& self) noexcept -> decltype(std::ranges::begin(self.m_data))
		{
			return std::ranges::begin(self.m_data);
		}

		[[nodiscard]]
		constexpr auto end(this auto&& self) noexcept -> decltype(std::ranges::end(self.m_data))
		{
			if (self.m_data[BACK] > BACK)
				return std::ranges::end(self.m_data);

			return std::ranges::begin(self.m_data) + self.m_data[BACK];
		}

		[[nodiscard]] constexpr auto cbegin(this auto&& self) noexcept -> decltype(self.begin()) { return self.begin(); }
		[[nodiscard]] constexpr auto cend(this auto&& self) noexcept -> decltype(self.end()) { return self.end(); }

		[[nodiscard]] constexpr size_t size(this auto&& self) noexcept
		{
			if (self.m_data[BACK] <= BACK)
				return self.m_data[BACK];

			return MAX_SIZE;
		}

		[[nodiscard]] constexpr bool empty() const noexcept { return m_data[BACK] == 0 || (m_data[0] == 0 && m_data[BACK] == static_cast<C>(-1)); }
		[[nodiscard]] constexpr auto data(this auto&& self) noexcept -> decltype(&self.m_data[0]) { return &self.m_data[0]; }

		[[nodiscard]] constexpr auto operator[](this auto&& self, size_t n) noexcept -> decltype(self.m_data[n]) { return self.m_data[n]; }

		[[nodiscard]] constexpr bool operator==(this auto&& lhs, multibytes_t rhs) noexcept
		{
			return std::bit_cast<uint32_t>(lhs) == std::bit_cast<uint32_t>(rhs);
		}
		[[nodiscard]] constexpr bool operator==(this auto&& lhs, auto&& rhs) noexcept requires(requires{ { rhs[0] } -> std::convertible_to<C>; })
		{
			if (std::ranges::size(rhs) < lhs.size())
				return false;

			for (auto&& [l, r] : std::views::zip(lhs, rhs))
			{
				if (l != static_cast<C>(r))
					return false;
			}

			return true;
		}

		C m_data[MAX_SIZE]{};
	};
}

// CType
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
	using u16char_t = std::conditional_t<sizeof(wchar_t) == sizeof(char16_t), wchar_t, char16_t>;

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
		using view_type = std::basic_string_view<std::conditional_t<sizeof(char_type) == 1, char, std::conditional_t<sizeof(char_type) == 2, u16char_t, fchar_t>>>;
		using owner_type = std::basic_string<std::conditional_t<sizeof(char_type) == 1, char, std::conditional_t<sizeof(char_type) == 2, u16char_t, fchar_t>>>;
		using traits_type = ::std::char_traits<std::conditional_t<sizeof(char_type) == 1, char, std::conditional_t<sizeof(char_type) == 2, u16char_t, fchar_t>>>;
		using multibytes_type = std::conditional_t<is_utf8, multibytes_t<char>, std::conditional_t<is_utf16, multibytes_t<u16char_t>, void>>;
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
		[[nodiscard]] static constexpr auto ToFullWidth(const_span_t bytes) noexcept -> fchar_t
		{
#ifndef _DEBUG
			if (std::ranges::empty(bytes))
				return 0x110000;	// Invalid Unicode point, max val is 0x10FFFF
#else
			assert(!std::ranges::empty(bytes));
#endif

			switch (CodePointOf(bytes.front()))
			{
			case CodePoint::WHOLE:
				return static_cast<fchar_t>(bytes.front());

			case CodePoint::BEGIN_OF_2:
			{
				assert(bytes.size() == 2 || (bytes.size() == 3 && bytes.back() == '\0'));

				if constexpr (is_utf8)
				{
					fchar_t ret = (bytes[0] & 0b00011111) << 6 | (bytes[1] & 0b00111111);

					if (ret < (fchar_t)0x80)		// Not a valid result, Wrong encoding
						ret = 0;					// Out of UTF8 bound, skip data  
					else if (ret > (fchar_t)0x7FF)	// Not a valid result, Wrong encoding
						ret = 0;					// Out of UTF8 bound, skip data

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
					assert(bytes.size() == 3 || (bytes.size() == 4 && bytes.back() == '\0'));
					fchar_t ret = (bytes[0] & 0b00001111) << 12 | (bytes[1] & 0b00111111) << 6 | (bytes[2] & 0b00111111);

					if (ret < (fchar_t)0x800)		// Not a valid result, Wrong encoding
						ret = 0;					// Out of UTF8 bound, skip data  
					else if (ret > (fchar_t)0xFFFF)	// Not a valid result, Wrong encoding
						ret = 0;					// Out of UTF8 bound, skip data  

					return ret;
				}

				[[fallthrough]];
			}

			case CodePoint::BEGIN_OF_4:
			{
				if constexpr (is_utf8)
				{
					assert(bytes.size() == 4 || (bytes.size() == 5 && bytes.back() == '\0'));
					fchar_t ret =
						(bytes[0] & 0b00000111) << 18 | (bytes[1] & 0b00111111) << 12 | (bytes[2] & 0b00111111) << 6 | (bytes[3] & 0b00111111);

					if (ret < (fchar_t)0x10000)			// Not a valid result, Wrong encoding
						ret = 0;						// Out of UTF8 bound, skip data  
					else if (ret > (fchar_t)0x10FFFF)	// Not a valid result, Wrong encoding 
						ret = 0;						// Out of UTF8 bound, skip data  

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
		[[nodiscard]] static constexpr auto ToMultiBytes(fchar_t wc) noexcept -> multibytes_type requires (is_utf8 || is_utf16)
		{
			multibytes_type res{};
			auto& ret = res.m_data;
			auto& cnt = res.m_data[res.BACK];

			if constexpr (is_utf8)
			{
				if (wc <= (fchar_t)0x7F)
				{
					ret[0] = static_cast<param_type>(wc);

					cnt = 1;
				}
				else if (wc <= (fchar_t)0x7FF)
				{
					ret[0] = static_cast<param_type>(0xC0u | (wc >> 6));			/* 110xxxxx */
					ret[1] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */

					cnt = 2;
				}
				else if (wc <= (fchar_t)0xFFFF) {
					ret[0] = static_cast<param_type>(0xE0u | (wc >> 12));			/* 1110xxxx */
					ret[1] = static_cast<param_type>(0x80u | ((wc >> 6) & 0x3Fu));	/* 10xxxxxx */
					ret[2] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */

					cnt = 3;
				}
				else if (wc <= (fchar_t)0x10FFFF) {
					ret[0] = static_cast<param_type>(0xF0u | (wc >> 18));			/* 11110xxx */
					ret[1] = static_cast<param_type>(0x80u | ((wc >> 12) & 0x3Fu));	/* 10xxxxxx */
					ret[2] = static_cast<param_type>(0x80u | ((wc >> 6) & 0x3Fu));	/* 10xxxxxx */
					ret[3] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */

					//cnt = 4;	// don't override the data! Now this cell is used as a part of UTF, rather than size indicator.
				}
				else
					cnt = -1;	// out of UTF range
			}
			else if constexpr (is_utf16)
			{
				if (wc < (fchar_t)0x10000)
				{
					ret[0] = static_cast<param_type>(wc);

					cnt = 1;
				}
				else if (wc <= (fchar_t)0x10FFFF)
				{
					ret[0] = static_cast<param_type>(((((uint32_t)wc - 0x10000u) << 12) >> 22) + 0xD800u);
					ret[1] = static_cast<param_type>(((((uint32_t)wc - 0x10000u) << 22) >> 22) + 0xDC00u);

					//cnt = 2;	// don't override the data! Now this cell is used as a part of UTF, rather than size indicator.
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

	// Unit testing at: UnitTest_UtlString_CType.cpp
}

// Misc, Part II
namespace Hydrogenium
{
	constexpr auto BuildStringView(auto&& it1, auto&& it2, auto&& end, bool do_offset) noexcept
	{
		static_assert(typeid(*it1) == typeid(*it2) && typeid(*end) == typeid(*it2));

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

	constexpr void MoveRevIterToUtfStartPos(ReverseIterator auto& it) noexcept
	{
		static_assert(!MultiWrappedRevIter<std::remove_cvref_t<decltype(it)>>, "Unwrap this multi-layer iterator first!");

		using CT = CType<decltype(*it)>;

		for (; CT::CodePointOf(*it) > CodePoint::BEGIN_OF_4; ++it) {}
	}

	// #MSVC_BUGGED_tailing_return_type_namespace_error

	// [0 ~ n]: counting from front; [-1 ~ -(n+1)]: counting from back.
	constexpr auto FetchBytes(std::ranges::input_range auto&& str, std::ptrdiff_t pos) noexcept
	{
		using CT = CType<decltype(str[0])>;
		auto ret = std::span<std::remove_reference_t<decltype(str[0])>>{};

		if (std::ranges::empty(str))
			return ret;

		if (pos >= 0)
		{
			auto it = std::ranges::begin(str);
			CodePoint cp{ CT::CodePointOf(*it) };

			while (it < std::ranges::end(str) && pos > 0)
			{
				cp = CT::CodePointOf(*it);
				assert(cp <= CodePoint::BEGIN_OF_4);

				it += (int)cp;
				--pos;
			}

			if (pos == 0 && it < std::ranges::end(str))
				return std::span{ std::addressof(*it), (size_t)cp, };

			return ret;	// Invalid Unicode point, max val is 0x10FFFF
		}
		else
		{
			pos = -pos;
			--pos;

			auto it = std::ranges::rbegin(str);
			MoveRevIterToUtfStartPos(it);

			CodePoint cp{ CT::CodePointOf(*it) };

			while (it < std::ranges::rend(str) && pos > 0)
			{
				do 
				{
					++it;
				} while ((cp = CT::CodePointOf(*it)) > CodePoint::BEGIN_OF_4);

				--pos;
			}

			if (pos == 0 && it < std::ranges::rend(str))
				return std::span{ std::addressof(*it), (size_t)cp, };

			return ret;	// Invalid Unicode point, max val is 0x10FFFF
		}

		std::unreachable();
	}

	constexpr fchar_t UtfAt(std::ranges::input_range auto&& str, std::ptrdiff_t pos) noexcept
	{
		using CT = CType<decltype(str[0])>;

		auto const bytes = FetchBytes(str, pos);
		return CT::ToFullWidth(bytes);
	}

	constexpr auto UtfSlicing(std::ranges::input_range auto&& str, int32_t pos1, std::optional<int16_t> pos2) noexcept
	{
		using CT = CType<decltype(str[0])>;

		auto const first = FetchBytes(str, pos1);
		auto const last = FetchBytes(str, pos2.value_or(-1));

		if (first.empty())
			return typename CT::view_type{};

		auto const p1 = std::addressof(first.front());
		auto const p2 = pos2 ? std::addressof(last.front()) : (std::ranges::data(str) + std::ranges::size(str));

		assert(p1 <= p2);
		return typename CT::view_type{ p1, p2 };
	}

	inline constexpr auto UTF8_TO_UTF16 =
		std::views::chunk_by([](auto, char rhs) { return CType<char>::CodePointOf(rhs) > CodePoint::BEGIN_OF_4; })
		| std::views::transform(&CType<char>::ToFullWidth)
		| std::views::transform(&CType<u16char_t>::ToMultiBytes)
		| std::views::join
		| std::ranges::to<std::basic_string>();

	inline constexpr auto UTF16_TO_UTF8 =
		std::views::chunk_by([](auto, u16char_t rhs) { return CType<u16char_t>::CodePointOf(rhs) > CodePoint::BEGIN_OF_4; })
		| std::views::transform(&CType<u16char_t>::ToFullWidth)
		| std::views::transform(&CType<char>::ToMultiBytes)
		| std::views::join
		| std::ranges::to<std::basic_string>();

	// Unit testing at: UnitTest_UtlString_Misc.cpp
}

namespace Hydrogenium::StringPolicy
{
	template <typename T>
	concept TypingPolicy = requires
	{
		typename T::char_type;
		typename T::param_type;
		typename T::view_type;
		typename T::owner_type;
		typename T::traits_type;

		{ typename T::view_type{} } -> std::ranges::input_range;
		{ typename T::owner_type{} } -> std::ranges::output_range<typename T::char_type>;

		requires std::is_constructible_v<typename T::view_type, typename T::owner_type>;
		requires std::is_constructible_v<typename T::owner_type, typename T::view_type>;
	};

	namespace Iterating
	{
		// Forward declearation.
		enum struct APRES : std::uint_fast8_t;
	}

	struct dummy_dir_mgr_t
	{
		static constexpr auto Begin(auto&& view) noexcept { return view.begin(); }
		static constexpr auto End(auto&& view) noexcept { return view.end(); }
		static inline constexpr bool is_reverse = false;
	};

	template <typename T>
	concept IteratingPolicy = requires (char* p, char const* cp, char* const pc, std::string_view view)
	{
		T::normal_pointer;	// enables random-access-iterator optimization.
		requires (requires{ T::Get(view, dummy_dir_mgr_t{}, ptrdiff_t{}); } || requires{ T::Get(view, ptrdiff_t{}); });	// dummy2 represents range policy.

		{ T::ValueOf(cp) } -> NonVoid;
		{ T::Arithmetic(p, pc, pc, ptrdiff_t{}) } -> std::same_as<Iterating::APRES>;
		{ T::NativeSize(p, pc) } -> std::integral;

		requires !requires{ T::Arithmetic(std::declval<char*>(), pc, pc, ptrdiff_t{}); }; // must not be able to handle xvalue or rvalue, as this is treat as if lvalue increment.
		{ T::ArithCpy(std::declval<char*>(), pc, pc, ptrdiff_t{}) } -> std::same_as<char*>;	// This one gives you a copy.
	};

	template <typename T>
	concept ComparingPolicy = requires (unsigned char c)
	{
		{ T{}.ChCmp(c, c) } -> std::same_as<int>;
		{ T{}.ChEql(c, c) } -> std::same_as<bool>;
	};

	template <typename T>
	concept DirectionPolicy = requires (std::string_view view)
	{
		{ T::Begin(view) } -> std::bidirectional_iterator;
		{ T::End(view) } -> std::bidirectional_iterator;
		{ T::Begin(view) } -> std::input_iterator;
		{ T::End(view) } -> std::input_iterator;
		T::is_reverse;
	};
	static_assert(DirectionPolicy<dummy_dir_mgr_t>);

	// Result post-processing
	template <typename R>
	concept ResultProcessor = requires (std::string_view sv, std::string s)
	{
		//R::Query(sv, sv, nullptr);	// it seems like there is no way we can fit a generalized 'iter policy' in.
		R::Test(int{});
		R::Counting(size_t{});
		{ R::Modify(s) } -> std::convertible_to<decltype(sv)>;
	};

	struct dummy_iter_policy_t final
	{
		static inline constexpr bool normal_pointer = false;
		static constexpr void Get(auto&&...) noexcept {}

		static constexpr char ValueOf(auto&&...) noexcept { return 0; }
		static constexpr Iterating::APRES Arithmetic(auto&, auto&&...) noexcept { return Iterating::APRES{}; }
		static constexpr int NativeSize(auto&&...) noexcept { return 0; }

		static constexpr auto ArithCpy(auto arg0, auto&&...) noexcept -> decltype(arg0) { return arg0; }
	};

	template <typename T>
	concept CountingPostProcessor = requires(size_t n) { { T::Transform(n) } -> NonVoid; };

	template <typename T>
	concept ModifyPostProcessor =
		requires(char const* const boundary) { { T::Transform(boundary, boundary, dummy_iter_policy_t{}, std::identity{}) } -> std::convertible_to<std::string_view>; }	// free style
		|| requires(char const* const boundary) { { T::Transform(boundary, boundary, std::identity{}) } -> std::convertible_to<std::string_view>; };	// component style

	template <typename T>
	concept QueryPostProcessor =
		requires(char const* const iter) { { T::Transform(iter, iter, iter, iter, iter, dummy_iter_policy_t{}) } -> NonVoid; }	// free style
		|| requires(char const* const iter) { { T::Transform(iter, iter, iter, iter, iter) } -> NonVoid; };	// component style

	template <typename T>
	concept TestPostProcessor = requires(int t) { { T::Transform(t) } -> NonVoid; };
}

namespace Hydrogenium::StringPolicy::Typing
{
	static_assert(TypingPolicy<CType<char>>);
	static_assert(TypingPolicy<CType<u16char_t>>);
	static_assert(TypingPolicy<CType<fchar_t>>);
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

	struct as_normal_ptr_t
	{
		static inline constexpr bool normal_pointer = true;

		static constexpr auto Get(auto&& view, StringPolicy::DirectionPolicy auto RangePolicy, ptrdiff_t size) noexcept
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

	struct as_multibytes_t
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

		static constexpr auto Get(auto&& view, StringPolicy::DirectionPolicy auto RangePolicy, ptrdiff_t size) noexcept
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

	static_assert(IteratingPolicy<as_normal_ptr_t>);
	static_assert(IteratingPolicy<as_multibytes_t>);

	// Unit testing at: UnitTest_UtlString_IterPolicy.cpp
}

namespace Hydrogenium::StringPolicy::Comparing
{
	struct case_ignored_t
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
		static constexpr int ChCmp(T lhs, U rhs) noexcept
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
		static constexpr bool ChEql(T lhs, U rhs) noexcept
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

	struct regular_t
	{
		[[nodiscard]]
		static constexpr int ChCmp(auto lhs, auto rhs) noexcept
		{
			return lhs - rhs;
		}

		[[nodiscard]]
		static constexpr bool ChEql(auto lhs, auto rhs) noexcept
		{
			return lhs == rhs;
		}
	};

	inline constexpr auto regular = regular_t{};

	static_assert(ComparingPolicy<case_ignored_t>);
	static_assert(ComparingPolicy<regular_t>);
}

namespace Hydrogenium::StringPolicy::Direction
{
	struct forwards_t
	{
		inline static constexpr bool is_reverse = false;

		static constexpr auto Begin(auto&& r) noexcept -> decltype(std::ranges::begin(r))
		{
			return std::ranges::begin(r);
		}

		static constexpr auto End(auto&& r) noexcept -> decltype(std::ranges::end(r))
		{
			return std::ranges::end(r);
		}
	};

	inline constexpr auto front_to_back = forwards_t{};

	struct backwards_t
	{
		inline static constexpr bool is_reverse = true;

		static constexpr auto Begin(auto&& r) noexcept -> decltype(std::ranges::rbegin(r))
		{
			return std::ranges::rbegin(r);
		}

		static constexpr auto End(auto&& r) noexcept -> decltype(std::ranges::rend(r))
		{
			return std::ranges::rend(r);
		}
	};

	inline constexpr auto back_to_front = backwards_t{};

	static_assert(DirectionPolicy<StringPolicy::Direction::forwards_t>);
	static_assert(DirectionPolicy<StringPolicy::Direction::backwards_t>);
}

namespace Hydrogenium::StringPolicy::Result
{
	struct as_it_is_t
	{
		using is_transparent = std::identity::is_transparent;	// make STL happy.

		// Chr, SpnP(Spn), PBrk(CSpn), Str
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(
			auto&& /*abs_begin*/, auto&& /*abs_end*/,
			auto&& /*rel_begin*/, std::bidirectional_iterator auto const& result_loc, auto&& /*rel_end*/,
			auto&& /*IterPolicy*/
		) noexcept
		{
			return result_loc;
		}

		// Cmp, Cnt(Len)
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(auto t) noexcept
			requires (std::is_trivial_v<std::remove_cvref_t<decltype(t)>>)
		{
			return t;
		}

		// Tok
		[[nodiscard]]
		__forceinline static constexpr decltype(auto) Transform(auto&& t) noexcept
			requires (!std::is_trivial_v<std::remove_cvref_t<decltype(t)>>)
		{
			// Move: to make sure that all resources were carry to caller.
			return std::move(t);
		}

		// Dup(Rev), Lwr, Upr => Unsupported.
	};

	// Chr, SpnP(Spn), PBrk(CSpn), Str
	struct as_pointer_t
	{
		[[nodiscard]]
		static constexpr auto Transform(auto&& /*abs_begin*/, auto&& /*abs_end*/, auto&& /*rel_begin*/, auto&& result_loc, auto&& rel_end, auto&& /*IterPolicy*/) noexcept
			-> decltype(std::addressof(*result_loc))
		{
			if (result_loc == rel_end)
				return nullptr;

			return std::addressof(*result_loc);
		}

		static constexpr auto UnitTestInvoke(auto&& result_loc, auto&& rel_end) noexcept
			-> decltype(Transform(nullptr, nullptr, nullptr, result_loc, rel_end, nullptr))
		{
			return Transform(
				nullptr,
				nullptr,
				nullptr,
				std::forward<decltype(result_loc)>(result_loc),
				std::forward<decltype(rel_end)>(rel_end),
				nullptr
			);
		}
	};
	struct as_indexing_t	// compatible with STL functions
	{
		[[nodiscard]]
		static constexpr auto Transform(auto&& abs_begin, auto&& /*abs_end*/, auto&& /*rel_begin*/, auto&& result_loc, auto&& rel_end, auto&& /*IterPolicy*/) noexcept
			-> std::ptrdiff_t
		{
			if (result_loc == rel_end)
				return std::numeric_limits<ptrdiff_t>::max();	// a.k.a. std::string::npos

			auto const fwd_result_loc = ToForwardIter(result_loc, result_loc != rel_end);

			return fwd_result_loc - abs_begin;
		}

		static constexpr auto UnitTestInvoke(auto&& abs_begin, auto&& result_loc, auto&& rel_end) noexcept
			-> decltype(Transform(abs_begin, nullptr, nullptr, result_loc, rel_end, nullptr))
		{
			return Transform(
				std::forward<decltype(abs_begin)>(abs_begin),
				nullptr,
				nullptr,
				std::forward<decltype(result_loc)>(result_loc),
				std::forward<decltype(rel_end)>(rel_end),
				nullptr
			);
		}
	};
	struct as_position_t	// intend to be used with Hydrogenium::UtfAt()
	{
		[[nodiscard]]
		static constexpr auto Transform(auto&& /*abs_begin*/, auto&& /*abs_end*/, auto&& rel_begin, auto&& result_loc, auto&& rel_end, StringPolicy::IteratingPolicy auto IterPolicy) noexcept
		{
			// cvref ignored.
			static_assert(typeid(rel_begin) == typeid(rel_end));

			constexpr bool bIsReverseIter = ReverseIterator<std::remove_cvref_t<decltype(result_loc)>>;
			ptrdiff_t count{};

			if constexpr (IterPolicy.normal_pointer)
			{
				// program is ill-formed if result_loc is not coming from rel_begin
				count = result_loc - rel_begin;
			}
			else
			{
				auto it = rel_begin;

				// In the reverse iter case, the rel_begin doesn't necessary being a startpos of UTF-Stream.
				if constexpr (bIsReverseIter)
					MoveRevIterToUtfStartPos(it);

				for (;
					it < result_loc;	// counting position from first all to found pos.
					IterPolicy.Arithmetic(it, rel_begin, rel_end, 1),
					++count)
				{
					// nothing to do.
				}
			}

			// Returning negative number representing 'indexing backwards', as in python.
			if constexpr (bIsReverseIter)
			{
				// starting counting from -1, upto -(length+1)
				++count;
				count = -count;
			}

			return count;
		}

		static constexpr auto UnitTestInvoke(auto&& rel_begin, auto&& result_loc, auto&& rel_end, StringPolicy::IteratingPolicy auto IterPolicy) noexcept
		{
			return Transform(
				nullptr,
				nullptr,
				std::forward<decltype(rel_begin)>(rel_begin),
				std::forward<decltype(result_loc)>(result_loc),
				std::forward<decltype(rel_end)>(rel_end),
				IterPolicy
			);
		}
	};
	struct as_view_t
	{
		[[nodiscard]]
		static constexpr auto Transform(auto&& /*abs_begin*/, auto&& /*abs_end*/, auto&& rel_begin, auto&& result_loc, auto&& rel_end, auto&& /*IterPolicy*/) noexcept
		{
			// cvref ignored.
			static_assert(typeid(rel_begin) == typeid(rel_end));

			auto fwd_rel_begin = ToForwardIter(rel_begin, false);
			auto fwd_rel_end = ToForwardIter(rel_end, false);
			auto fwd_res_loc = ToForwardIter(result_loc, result_loc != rel_end);

			if (fwd_rel_begin > fwd_rel_end)	// begin should always less than end.
				std::swap(fwd_rel_begin, fwd_rel_end);

			if (result_loc == rel_end)	// no found!
				fwd_res_loc = fwd_rel_end;

			return typename CType<decltype(*result_loc)>::view_type{ fwd_res_loc, fwd_rel_end };
		}

		static constexpr auto UnitTestInvoke(auto&& rel_begin, auto&& result_loc, auto&& rel_end) noexcept
		{
			return Transform(
				nullptr,
				nullptr,
				std::forward<decltype(rel_begin)>(rel_begin),
				std::forward<decltype(result_loc)>(result_loc),
				std::forward<decltype(rel_end)>(rel_end),
				nullptr
			);
		}
	};

	static_assert(QueryPostProcessor<as_it_is_t>);
	static_assert(QueryPostProcessor<as_pointer_t>);
	static_assert(QueryPostProcessor<as_indexing_t>);
	static_assert(QueryPostProcessor<as_position_t>);
	static_assert(QueryPostProcessor<as_view_t>);

	// Cmp
	struct as_lexic_t
	{
		[[nodiscard]]
		static constexpr int Transform (std::same_as<int> auto val) noexcept
		{
			return val;
		}
	};
	struct as_stl_ordering_t
	{
		[[nodiscard]]
		static constexpr std::strong_ordering Transform (std::same_as<int> auto val) noexcept
		{
			if (val < 0)
				return std::strong_ordering::less;
			else if (val > 0)
				return std::strong_ordering::greater;
			else
				return std::strong_ordering::equal;	// that's what expression 0<=>0 will return.
		}
	};
	struct as_eql_t final { [[nodiscard]] static constexpr bool Transform(std::same_as<int> auto val) noexcept { return val == 0; } };
	struct as_lt_t final { [[nodiscard]] static constexpr bool Transform(std::same_as<int> auto val) noexcept { return val < 0; } };
	struct as_lt_eq_t final { [[nodiscard]] static constexpr bool Transform(std::same_as<int> auto val) noexcept { return val <= 0; } };
	struct as_gt_t final { [[nodiscard]] static constexpr bool Transform(std::same_as<int> auto val) noexcept { return val > 0; } };
	struct as_gt_eq_t final { [[nodiscard]] static constexpr bool Transform(std::same_as<int> auto val) noexcept { return val >= 0; } };
	struct as_not_eq_t final { [[nodiscard]] static constexpr bool Transform(std::same_as<int> auto val) noexcept { return val != 0; } };

	static_assert(TestPostProcessor<as_it_is_t>);
	static_assert(TestPostProcessor<as_lexic_t>);
	static_assert(TestPostProcessor<as_stl_ordering_t>);

	// Cnt(Len) #UPDATE_AT_CPP26 sat_cast
	struct as_signed_t
	{
		[[nodiscard]]
		static constexpr auto Transform(std::same_as<size_t> auto n) noexcept -> std::make_signed_t<size_t>
		{
			n = std::min(n, (size_t)std::numeric_limits<std::make_signed_t<size_t>>::max());
			return static_cast<std::make_signed_t<size_t>>(n);
		}
	};
	struct as_unsigned_t
	{
		[[nodiscard]]
		static constexpr size_t Transform(std::same_as<size_t> auto n) noexcept
		{
			return n;
		}
	};

	static_assert(CountingPostProcessor<as_it_is_t>);
	static_assert(CountingPostProcessor<as_signed_t>);
	static_assert(CountingPostProcessor<as_unsigned_t>);

	// Dup(Rev), Lwr, Upr
	struct as_unmanaged_t
	{
		template <typename proj_t = std::identity> [[nodiscard]]
		static constexpr auto Transform(auto&& first, auto&& last, auto IterPolicy, proj_t&& proj = {}) noexcept
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

					auto const bytes = CType<char_type>::ToMultiBytes(proj(IterPolicy.ValueOf(it)));

					for (auto&& byt : bytes)
					{
						p[i++] = byt;
					}
				}
			}

			return p;
		}
	};
	struct as_marshaled_t
	{
		template <typename proj_t = std::identity> [[nodiscard]]
		static constexpr auto Transform(auto&& first, auto&& last, auto IterPolicy, proj_t&& proj = {}) noexcept
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

					ret.append_range(
						CType<char_type>::ToMultiBytes(proj(IterPolicy.ValueOf(it)))
					);
				}
			}

			return ret;
		}
	};

	static_assert(ModifyPostProcessor<as_unmanaged_t>);
	static_assert(ModifyPostProcessor<as_marshaled_t>);	// this one is the default, as_it_is_t doesn't applys here.

	// Tok
	struct as_generator_t final {};	// #UPDATE_AT_CPP23 generator
	struct as_vector_t final {};
	inline constexpr auto as_generator = as_generator_t{};
	inline constexpr auto as_vector = as_vector_t{};
}

namespace Hydrogenium::String::Functors::Components
{
#define REQ_TYPE_INFO	static_assert(StringPolicy::TypingPolicy<Base>, "Requires a type info component!")
#define REQ_DIR_MGR		static_assert(StringPolicy::DirectionPolicy<Base>, "Requires a direction manager component!")
#define REQ_ITER_MGR	static_assert(StringPolicy::IteratingPolicy<Base>, "Requires a iterator manager component!")
#define REQ_COMPARATOR	static_assert(StringPolicy::ComparingPolicy<Base>, "Requires a comparator!")
#define REQ_QUERY_PP	static_assert(StringPolicy::QueryPostProcessor<Base>, "Requires a query-functor compatible postprocessor!")
#define REQ_MODIFY_PP	static_assert(StringPolicy::ModifyPostProcessor<Base>, "Requires a modifying-functor compatible postprocessor!")

	inline constexpr auto MAX_COUNT = std::numeric_limits<std::ptrdiff_t>::max();

	template <typename, typename Base>
	struct alg_chr : Base
	{
		REQ_TYPE_INFO;
		using typename Base::param_type;
		using typename Base::view_type;

		REQ_ITER_MGR;
		using Base::Arithmetic;
		using Base::Get;
		using Base::ValueOf;

		REQ_COMPARATOR;
		using Base::ChEql;

		REQ_QUERY_PP;
		using Base::Transform;

		using Base::operator();	// Just watch out for overload resolution

		[[nodiscard]]
		constexpr auto operator()(view_type str, param_type ch, ptrdiff_t until = MAX_COUNT) const noexcept
			-> decltype(Transform(str.begin(), str.end(), str.end()))
		{
			auto [begin, it, end] = Get(str, until);

			for (; it < end; Arithmetic(it, begin, end, 1))
			{
				if (ChEql(ValueOf(it), ch))
					break;
			}

			// This is absolute begin and end.
			return Transform(str.begin(), str.end(), begin, it, end);
		}
	};

	template <typename, typename Base>
	struct alg_cmp : Base
	{
		REQ_TYPE_INFO;
		using typename Base::value_type;
		using typename Base::view_type;

		REQ_ITER_MGR;
		using Base::Arithmetic;
		using Base::Get;
		using Base::ValueOf;

		REQ_COMPARATOR;
		using Base::ChEql;
		using Base::ChCmp;

		static inline constexpr bool HAS_TEST_POSTPROCESSOR = StringPolicy::TestPostProcessor<Base>;
		using transformed_ret_type = std::invoke_result_t<decltype([] { if constexpr (HAS_TEST_POSTPROCESSOR) return Base::Transform(int{}); else return int{}; })>;

		using Base::operator();	// Just watch out for overload resolution

		[[nodiscard]]
		constexpr transformed_ret_type operator()(view_type lhs, view_type rhs, ptrdiff_t count = MAX_COUNT) const noexcept
		{
			auto [b1, s1, e1] = Get(lhs, count);
			auto [b2, s2, e2] = Get(rhs, count);

			while (
				s1 < e1 && s2 < e2
				&& ChEql(ValueOf(s1), ValueOf(s2))
				)
			{
				Arithmetic(s1, b1, e1, 1);
				Arithmetic(s2, b2, e2, 1);
			}

			// Preventing deducing as something like 'int32_t'
			value_type const c1 = s1 == e1 ? '\0' : ValueOf(s1);
			value_type const c2 = s2 == e2 ? '\0' : ValueOf(s2);

			if constexpr (HAS_TEST_POSTPROCESSOR)
			{
				return Transform(ChCmp(c1, c2));
			}
			else
			{
				return ChCmp(c1, c2);
			}
		}
	};

	template <typename, typename Base>
	struct alg_cnt : Base
	{
		REQ_TYPE_INFO;
		using typename Base::view_type;

		REQ_ITER_MGR;
		using Base::Arithmetic;
		using Base::Get;

		static inline constexpr bool HAS_COUNTING_POSTPROCESSOR = StringPolicy::CountingPostProcessor<Base>;
		using transformed_ret_type = std::invoke_result_t<decltype([] { if constexpr (HAS_COUNTING_POSTPROCESSOR) return Base::Transform(size_t{}); else return size_t{}; })>;

		using Base::operator();	// Just watch out for overload resolution

		[[nodiscard]]
		constexpr transformed_ret_type operator()(view_type const& str, ptrdiff_t count = MAX_COUNT) const noexcept
		{
			auto [begin, it, end] = Get(str, count);

			// Count is length, same impl, just different on ptr arithmetic.
			size_t n{};
			for (; it < end; Arithmetic(it, begin, end, 1)) { ++n; }

			if constexpr (HAS_COUNTING_POSTPROCESSOR)
			{
				return Base::Transform(n);
			}
			else
			{
				return n;
			}
		}
	};

	template <typename, typename Base>
	struct alg_dup : Base
	{
		REQ_TYPE_INFO;
		using typename Base::view_type;

		REQ_ITER_MGR;
		using Base::Get;

		REQ_MODIFY_PP;
		using Base::Transform;

		using Base::operator();	// Just watch out for overload resolution

		[[nodiscard]]
		constexpr auto operator()(view_type const& str, ptrdiff_t count = MAX_COUNT) const noexcept
			-> decltype(Transform(str.begin(), str.end()))
		{
			auto [_, it, end] = Get(str, count);

			// the count param represents count of graphemes.
			return Transform(it, end);
		}
	};

	template <typename, typename Base, bool bSpnPMode>
	struct impl_alg_find : Base
	{
		REQ_TYPE_INFO;
		using typename Base::view_type;

		REQ_ITER_MGR;
		using Base::Arithmetic;
		using Base::Get;
		using Base::ValueOf;

		REQ_COMPARATOR;
		using Base::ChEql;

		REQ_QUERY_PP;
		using Base::Transform;

		[[nodiscard]]
		constexpr auto operator()(view_type str, view_type charset, ptrdiff_t count = MAX_COUNT) const noexcept
		{
			// The count is capping the str, not charset
			auto [b1, s1, e1] = this->Get(str, count);
			auto const b2 = charset.begin(), e2 = charset.end();	// And it's not directional. It's a set.

			for (; s1 < e1; Arithmetic(s1, b1, e1, 1))
			{
				bool found_in_src = false;
				auto const ch1 = ValueOf(s1);

				for (auto s2 = b2; s2 < e2; Arithmetic(s2, b2, e2, 1))
				{
					auto const ch2 = ValueOf(s2);

					if (ChEql(ch1, ch2))
					{
						found_in_src = true;
						break;
					}
				}

				if (found_in_src == !bSpnPMode)
					break;
			}

			return Transform(str.begin(), str.end(), b1, s1, e1);
		}
	};

	template <typename CFinal, typename Base>
	using alg_pbrk = impl_alg_find<CFinal, Base, false>;

	template <typename CFinal, typename Base>
	using alg_spnp = impl_alg_find<CFinal, Base, true>;

#undef REQ_TYPE_INFO
#undef REQ_DIR_MGR
#undef REQ_ITER_MGR
#undef REQ_COMPARATOR
#undef REQ_QUERY_PP
#undef REQ_MODIFY_PP
}

namespace Hydrogenium::String::Functors::Components
{
	struct base_comp_t
	{
		// Only for non-dependented name, quick diagnosis.
		// https://en.cppreference.com/w/cpp/language/dependent_name
		// Just keep compiler happy.
		void operator()(...) const noexcept = delete;
	};

	struct empty_comp_t {};

	template <typename CFinal, template <typename, typename> class TFirst, template <typename, typename> class... TRests>
	struct Linker : TFirst<CFinal, Linker<CFinal, TRests...>> { using TFirst<CFinal, Linker<CFinal, TRests...>>::operator(); };

	template <typename CFinal, template <typename, typename> class TFirst>
	struct Linker<CFinal, TFirst> : TFirst<CFinal, base_comp_t> { using TFirst<CFinal, base_comp_t>::operator(); };

#pragma region Type info

	template <typename CFinal, typename Base, typename T>
	struct wrapper_info : Base
	{
		using policy_info = wrapper_info;
		static_assert(!requires{ typename Base::policy_info; }, "Only one type policy allowed!");

		using ctype_info = CType<T>;
		using char_type = std::remove_cvref_t<T>;
		using param_type = std::conditional_t<sizeof(char_type) == 1, unsigned char, std::conditional_t<sizeof(char_type) == 2, u16char_t, fchar_t>>;
		using view_type = ::std::basic_string_view<std::conditional_t<sizeof(char_type) == 1, char, std::conditional_t<sizeof(char_type) == 2, u16char_t, fchar_t>>>;
		using owner_type = ::std::basic_string<std::conditional_t<sizeof(char_type) == 1, char, std::conditional_t<sizeof(char_type) == 2, u16char_t, fchar_t>>>;
		using traits_type = ::std::char_traits<std::conditional_t<sizeof(char_type) == 1, char, std::conditional_t<sizeof(char_type) == 2, u16char_t, fchar_t>>>;
	};

	template <typename CFinal, typename Base>
	using info_u8 = wrapper_info<CFinal, Base, char>;

	template <typename CFinal, typename Base>
	using info_u16 = wrapper_info<CFinal, Base, u16char_t>;

	template <typename CFinal, typename Base>
	using info_u32 = wrapper_info<CFinal, Base, fchar_t>;

	static_assert(StringPolicy::TypingPolicy<info_u8<base_comp_t, base_comp_t>>);
	static_assert(StringPolicy::TypingPolicy<info_u16<base_comp_t, base_comp_t>>);
	static_assert(StringPolicy::TypingPolicy<info_u32<base_comp_t, base_comp_t>>);

#pragma endregion Type info

#pragma region Iterating

	template <typename CFinal, typename Base, StringPolicy::IteratingPolicy CWrapped>
	struct wrapper_iter : Base, CWrapped
	{
		static_assert(!requires{ typename Base::policy_iter; }, "Only one iterator policy allowed!");
		using policy_iter = CWrapped;

		static_assert(requires{ typename Base::char_type; typename Base::view_type; }, "Requires a type info component before introducing an iterator policy!");
		using typename Base::char_type;
		using typename Base::view_type;

		static_assert(requires{ typename Base::policy_dir; }, "Requires a search direction component before introducing an iterator policy!");
		using typename Base::policy_dir;

		using CWrapped::normal_pointer;

		using CWrapped::ValueOf;
		using CWrapped::Arithmetic;
		using CWrapped::ArithCpy;
		using CWrapped::NativeSize;

		static inline constexpr auto Get =
			[](view_type const& view, ptrdiff_t until = std::numeric_limits<ptrdiff_t>::max()) noexcept -> decltype(CWrapped::Get(view, policy_dir{}, until))
			{
				return CWrapped::Get(view, policy_dir{}, until);
			};
	};

	template <typename CFinal, typename Base>
	using iter_default = wrapper_iter<CFinal, Base, StringPolicy::Iterating::as_normal_ptr_t>;

	template <typename CFinal, typename Base>
	using iter_multibytes = wrapper_iter<CFinal, Base, StringPolicy::Iterating::as_multibytes_t>;

#pragma endregion Iterating

#pragma region Comparing

	template <typename CFinal, typename Base, StringPolicy::ComparingPolicy CWrapped>
	struct wrapper_cmp : Base, CWrapped
	{
		static_assert(!requires{ typename Base::policy_cmp; }, "Only one comparing policy allowed!");
		using policy_cmp = CWrapped;

		using CWrapped::ChCmp;
		using CWrapped::ChEql;
	};

	template <typename CFinal, typename Base>
	using cmp_default = wrapper_cmp<CFinal, Base, StringPolicy::Comparing::regular_t>;

	template <typename CFinal, typename Base>
	using cmp_case_ignored = wrapper_cmp<CFinal, Base, StringPolicy::Comparing::case_ignored_t>;

#pragma endregion Comparing

#pragma region Direction

	template <typename CFinal, typename Base, StringPolicy::DirectionPolicy CWrapped>
	struct wrapper_dir : Base, CWrapped
	{
		static_assert(!requires{ typename Base::policy_dir; }, "Only one directional policy allowed!");
		using policy_dir = CWrapped;

		using CWrapped::is_reverse;

		using CWrapped::Begin;
		using CWrapped::End;
	};

	template <typename CFinal, typename Base>
	using dir_forward = wrapper_dir<CFinal, Base, StringPolicy::Direction::forwards_t>;

	template <typename CFinal, typename Base>
	using dir_backward = wrapper_dir<CFinal, Base, StringPolicy::Direction::backwards_t>;

#pragma endregion Direction

#pragma region Result

	template <typename CFinal, typename Base, typename CWrapped>
	struct wrapper_ret : Base, CWrapped
	{
		using is_transparent = std::identity::is_transparent;	// make STL happy.

		static_assert(!requires{ typename Base::policy_ret; }, "Only one returning transformer allowed!");
		using policy_ret = CWrapped;

		static_assert(requires{ typename Base::policy_iter; }, "Requires a iterator manager component before introducing an returning transformer!");
		using typename Base::policy_iter;

		// Query

		[[nodiscard]]
		__forceinline static constexpr auto Transform(
			std::contiguous_iterator auto const& abs_begin, std::contiguous_iterator auto const& abs_end,
			std::bidirectional_iterator auto const& rel_begin, std::bidirectional_iterator auto const& result_loc, std::bidirectional_iterator auto const& rel_end
		) noexcept -> decltype(CWrapped::Transform(abs_begin, abs_end, rel_begin, result_loc, rel_end, policy_iter{}))
		{
			return CWrapped::Transform(
				abs_begin, abs_end,
				rel_begin, result_loc, rel_end,
				policy_iter{}
			);
		}

		// Test & Counting

		[[nodiscard]]
		__forceinline static constexpr auto Transform(std::integral auto val) noexcept -> decltype(CWrapped::Transform(val))
		{
			return CWrapped::Transform(val);
		}

		// Modify

		template <typename proj_t = std::identity>
		[[nodiscard]]
		__forceinline static constexpr auto Transform(
			std::bidirectional_iterator auto const& first, std::bidirectional_iterator auto const& last, proj_t&& proj = {}
		) noexcept -> decltype(CWrapped::Transform(first, last, policy_iter{}, std::move(proj))) requires (requires{ std::invoke(proj, policy_iter::ValueOf(*first)); })
		{
			return CWrapped::Transform(first, last, policy_iter{}, std::move(proj));
		}
	};

	// Result: Common

	template <typename CFinal, typename Base>
	using ret_as_it_is = wrapper_ret<CFinal, Base, StringPolicy::Result::as_it_is_t>;

	// Result: Chr, SpnP(Spn), PBrk(CSpn), Str

	template <typename CFinal, typename Base>
	using ret_as_pointer = wrapper_ret<CFinal, Base, StringPolicy::Result::as_pointer_t>;

	template <typename CFinal, typename Base>
	using ret_as_index = wrapper_ret<CFinal, Base, StringPolicy::Result::as_indexing_t>;

	template <typename CFinal, typename Base>
	using ret_as_position = wrapper_ret<CFinal, Base, StringPolicy::Result::as_position_t>;

	template <typename CFinal, typename Base>
	using ret_as_view = wrapper_ret<CFinal, Base, StringPolicy::Result::as_view_t>;

	// Result: Cmp

	template <typename CFinal, typename Base>
	using ret_as_stl_ordering = wrapper_ret<CFinal, Base, StringPolicy::Result::as_stl_ordering_t>;

	// Result: Cnt(Len) #UPDATE_AT_CPP26 sat_cast

	template <typename CFinal, typename Base>
	using ret_as_signed = wrapper_ret<CFinal, Base, StringPolicy::Result::as_signed_t>;

	// Result: Dup(Rev), Lwr, Upr

	template <typename CFinal, typename Base>
	using ret_as_unmanaged = wrapper_ret<CFinal, Base, StringPolicy::Result::as_unmanaged_t>;

	template <typename CFinal, typename Base>
	using ret_as_marshaled = wrapper_ret<CFinal, Base, StringPolicy::Result::as_marshaled_t>;

	// Result: Tok

	// I don't think it is doable.
	// It's of lazy and active difference.

#pragma endregion Result

}

namespace Hydrogenium::String
{
	template <
		typename char_type = char,
		StringPolicy::IteratingPolicy IterPolicy = StringPolicy::Iterating::as_normal_ptr_t,
		StringPolicy::ComparingPolicy Comparator = StringPolicy::Comparing::regular_t,
		StringPolicy::DirectionPolicy RangePolicy = StringPolicy::Direction::forwards_t,
		StringPolicy::QueryPostProcessor QueryPostProc = StringPolicy::Result::as_view_t,
		StringPolicy::TestPostProcessor TestPostProc = StringPolicy::Result::as_it_is_t,
		StringPolicy::CountingPostProcessor CountingPostProc = StringPolicy::Result::as_it_is_t,
		StringPolicy::ModifyPostProcessor ModifyPostProc = StringPolicy::Result::as_marshaled_t
	>
	struct Utils final
	{
		using ctype_info = CType<char_type>;
		using nullable_type = std::optional<typename ctype_info::view_type>;
		using owner_type = ctype_info::owner_type;
		using owner_iter = decltype(RangePolicy::Begin(owner_type{}));
		using param_type = ctype_info::param_type;
		using value_type = decltype(IterPolicy::ValueOf(owner_iter{}));
		using view_type = ctype_info::view_type;
		using view_iter = decltype(RangePolicy::Begin(view_type{}));

		struct detail final
		{
			static inline constexpr auto default_search_len = std::numeric_limits<ptrdiff_t>::max();

			// Chr - v, c -> string_view; No internal impl needed.
			// Cmp - v, v -> int; No internal impl needed.

			// Cnt - v -> size_t; Counting graphemes in a char[] range.
			__forceinline static constexpr size_t Cnt(view_type const& str, ptrdiff_t count = default_search_len) noexcept
			{
				auto [begin, it, end] = IterPolicy::Get(str, RangePolicy{}, count);

				size_t n = begin == end ? 0 : 1;	// #UPDATE_AT_CPP23 size type literal
				for (; IterPolicy::Arithmetic(it, begin, end, 1) & StringPolicy::Iterating::APRES::MOVED; ++n) {}

				return n;
			}

			// CSpn - v, v -> size_t; Use PBrk instead. This one will count distance from RangePolicy.Begin() instead of from absolute start. 'R' stands for 'relative'.
			static constexpr ptrdiff_t CSpnR(view_type const& dest, view_type const& src, ptrdiff_t count = default_search_len, bool bSpnMode = false) noexcept
			{
				// the src is NOT order-sensitive, it's no more than a set of what to search.
				// hence no range function put onto src.

				auto [b1, s1, e1] = IterPolicy::Get(dest, RangePolicy{}, count);
				auto [b2, s2, e2] = IterPolicy::Get(src, StringPolicy::Direction::front_to_back, default_search_len);
				ptrdiff_t counter = 0;

				for (; s1 < e1; IterPolicy::Arithmetic(s1, b1, e1, 1), ++counter)
				{
					bool found_in_src = false;
					auto const ch1 = IterPolicy::ValueOf(s1);

					for (s2 = b2; s2 < e2; IterPolicy::Arithmetic(s2, b2, e2, 1))
					{
						auto const ch2 = IterPolicy::ValueOf(s2);

						if (Comparator::ChEql(ch1, ch2))
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
				auto [begin, it, end] = IterPolicy::Get(str, RangePolicy{}, count);

				if constexpr (!RangePolicy::is_reverse)
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
			// Lwr - o -> string; No internal impl needed.

			// PBrk - v, v -> string_view; served as CSpn, Spn, SpnP as well.
			__forceinline static constexpr auto PBrk(view_type const& dest, view_type const& src, ptrdiff_t count = default_search_len, bool bSpnPMode = false) noexcept
			{
				// the src is NOT order-sensitive, it's no more than a set of what to search.
				// hence no range function put onto src.

				auto [b1, s1, e1] = IterPolicy::Get(dest, RangePolicy{}, count);
				auto [b2, s2, e2] = IterPolicy::Get(src, StringPolicy::Direction::front_to_back, default_search_len);

				for (; s1 < e1; IterPolicy::Arithmetic(s1, b1, e1, 1))
				{
					bool found_in_src = false;
					auto const ch1 = IterPolicy::ValueOf(s1);

					for (s2 = b2; s2 < e2; IterPolicy::Arithmetic(s2, b2, e2, 1))
					{
						auto const ch2 = IterPolicy::ValueOf(s2);

						if (Comparator::ChEql(ch1, ch2))
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
				IterPolicy::Arithmetic(it, begin, end, org_before_comp);

				if (it >= end)
					return BuildStringView(end, end, end, false);

				// II. Move pointer to next delim position. And now 'it' serves as end.
				auto const tokenBegin = it;
				auto const org_before_comp2 = CSpnR(BuildStringView(it, end, end, false), delim);
				IterPolicy::Arithmetic(it, begin, end, org_before_comp2);

				// buffer is the end in this section as it is going to be assigned as '\0' in original strtok().
				return BuildStringView(tokenBegin, it, end, false);
			}

			// Upr - o -> string; No internal impl needed.
		};

#pragma region Chr
		struct chr_fn_t
		{
			constexpr auto operator()(view_type const& str, param_type ch, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(QueryPostProc::Transform(str.begin(), str.end(), RangePolicy::Begin(str), RangePolicy::End(str), RangePolicy::End(str), IterPolicy{}))
			{
				auto [begin, it, end] = IterPolicy::Get(str, RangePolicy{}, until);

				for (; it < end; IterPolicy::Arithmetic(it, begin, end, 1))
				{
					if (Comparator::ChEql(IterPolicy::ValueOf(it), ch))
						break;
				}

				// This is absolute begin and end.
				return QueryPostProc::Transform(
					str.begin(), str.end(), begin, it, end,
					IterPolicy{}
				);
			}
		};
		static inline constexpr auto Chr = chr_fn_t{};
#pragma endregion Chr

#pragma region Cmp
		struct cmp_fn_t
		{
			constexpr auto operator()(view_type const& lhs, view_type const& rhs, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(TestPostProc::Transform(0))
			{
				auto [b1, s1, e1] = IterPolicy::Get(lhs, RangePolicy{}, count);
				auto [b2, s2, e2] = IterPolicy::Get(rhs, RangePolicy{}, count);

				while (
					s1 < e1 && s2 < e2
					&& Comparator::ChEql(IterPolicy::ValueOf(s1), IterPolicy::ValueOf(s2))
					)
				{
					IterPolicy::Arithmetic(s1, b1, e1, 1);
					IterPolicy::Arithmetic(s2, b2, e2, 1);
				}

				// Preventing deducing as something like 'int32_t'
				value_type const c1 = s1 == e1 ? '\0' : IterPolicy::ValueOf(s1);
				value_type const c2 = s2 == e2 ? '\0' : IterPolicy::ValueOf(s2);

				return TestPostProc::Transform(
					Comparator::ChCmp(c1, c2)
				);
			}
		};
		static inline constexpr auto Cmp = cmp_fn_t{};
#pragma endregion Cmp

#pragma region Cnt
		struct cnt_fn_t
		{
			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(CountingPostProc::Transform(size_t{}))
			{
				return CountingPostProc::Transform(
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
				-> decltype(ModifyPostProc::Transform(RangePolicy::Begin(str), RangePolicy::End(str), IterPolicy{}))
			{
				auto [_, it, logical_end] = IterPolicy::Get(str, RangePolicy{}, count);

				// the count param is for size in the native type. Not count of graphemes.
				return ModifyPostProc::Transform(it, logical_end, IterPolicy{});
			}
		};
		static inline constexpr auto Dup = dup_fn_t{};
#pragma endregion Dup

#pragma region Lwr
		struct lwr_fn_t
		{
			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(ModifyPostProc::Transform(str.begin(), str.end(), IterPolicy{}))
			{
				auto [_, it, end] = IterPolicy::Get(str, RangePolicy{}, count);

				return ModifyPostProc::Transform(it, end, IterPolicy{}, &CType<value_type>::ToLower);
			}

			// return type is void, in the case of in_place mode.
			constexpr void operator()(owner_type* pstr, ptrdiff_t count = detail::default_search_len) const noexcept
			{
			// explictly create a view, such that compiler won't complain about const& object expiring.
				view_type const str{ *pstr };
				auto [_, it, end] = IterPolicy::Get(str, RangePolicy{}, count);

				static_assert(
					typeid(ModifyPostProc::Transform(it, end, IterPolicy{}, &CType<value_type>::ToLower)) == typeid(owner_type),
					"Lwr() method must be used with marshaled returning types."
				);

				*pstr = ModifyPostProc::Transform(it, end, IterPolicy{}, &CType<value_type>::ToLower);
			}
		};
		static inline constexpr auto Lwr = lwr_fn_t{};
#pragma endregion Lwr

#pragma region PBrk
		struct pbrk_fn_t
		{
			constexpr auto operator()(view_type const& dest, view_type const& src, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(QueryPostProc::Transform(dest.begin(), dest.end(), RangePolicy::Begin(dest), RangePolicy::End(dest), RangePolicy::End(dest), IterPolicy{}))
			{
				auto [begin, end, it] = detail::PBrk(dest, src, count, false);

				return QueryPostProc::Transform(
					dest.begin(), dest.end(), begin, it, end,
					IterPolicy{}
				);
			}
		};
		static inline constexpr auto PBrk = pbrk_fn_t{};
#pragma endregion PBrk

#pragma region SpnP
		struct spnp_fn_t
		{
			constexpr auto operator()(view_type const& dest, view_type const& src, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(QueryPostProc::Transform(dest.begin(), dest.end(), RangePolicy::Begin(dest), RangePolicy::End(dest), RangePolicy::End(dest), IterPolicy{}))
			{
				auto [begin, end, it] = detail::PBrk(dest, src, count, true);

				return QueryPostProc::Transform(
					dest.begin(), dest.end(), begin, it, end,
					IterPolicy{}
				);
			}
		};
		static inline constexpr auto SpnP = spnp_fn_t{};
#pragma endregion SpnP

#pragma region Str
		struct str_fn_t
		{
			constexpr auto operator()(view_type const& str, view_type const& substr, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(QueryPostProc::Transform(str.begin(), str.end(), RangePolicy::Begin(str), RangePolicy::End(str), RangePolicy::End(str), IterPolicy{}))
			{
				// Searching direction is nothing to do with comparing direction!!
				// the substr is going to attempting to match with the forwarding order.
				// hence no range function put onto src.

				auto [b1, s1, e1] = IterPolicy::Get(str, RangePolicy{}, until);
				auto const iSubstrCnt = detail::Cnt(substr);

				constexpr auto FwdCmp = Utils<
					char_type,
					IterPolicy,
					Comparator,
					StringPolicy::Direction::forwards_t,
					QueryPostProc,
					StringPolicy::Result::as_it_is_t	// To guarantee a lexicographical result.
				>::Cmp;

				if constexpr (!RangePolicy::is_reverse)
				{
					for (; s1 < e1; IterPolicy::Arithmetic(s1, b1, e1, 1))
					{
						if (FwdCmp({ s1, e1 }, substr, iSubstrCnt) == 0)
							break;
					}
				}
				else
				{
					for (; s1 < e1; IterPolicy::Arithmetic(s1, b1, e1, 1))
					{
						auto const fwit1 = ToForwardIter(s1);
						auto const fwed1 = ToForwardIter(b1, false);	// in reverse_iter, rbegin is the actual end.

						if (FwdCmp({ fwit1, fwed1 }, substr, iSubstrCnt) == 0)
							break;
					}
				}

				return QueryPostProc::Transform(
					str.begin(), str.end(), b1, s1, e1,
					IterPolicy{}
				);
			}
		};
		static inline constexpr auto Str = str_fn_t{};
#pragma endregion Str

#pragma region Tok
		struct tok_fn_t
		{
			auto operator()(nullable_type const& psz, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
			{
				static thread_local std::tuple_element_t<0, decltype(IterPolicy::Get(*psz, RangePolicy{}, until))> begin{};
				static thread_local std::tuple_element_t<1, decltype(IterPolicy::Get(*psz, RangePolicy{}, until))> it{};
				static thread_local std::tuple_element_t<2, decltype(IterPolicy::Get(*psz, RangePolicy{}, until))> end{};

				if (psz.has_value())
					std::tie(begin, it, end) = IterPolicy::Get(*psz, RangePolicy{}, until);	// in all other calling case, 'until' param will be ignored.

				return detail::Tok(begin, it, end, delim);
			}

			auto operator()(StringPolicy::Result::as_generator_t, view_type const& str, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
				-> GENERATOR_TY<view_type>
			{
				auto [begin, it, end] = IterPolicy::Get(str, RangePolicy{}, until);

				for (auto view = detail::Tok(begin, it, end, delim); !view.empty(); view = detail::Tok(begin, it, end, delim))
				{
					co_yield view;
				}

				co_return;
			}

			constexpr auto operator()(StringPolicy::Result::as_vector_t, view_type const& str, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
				-> std::vector<view_type>
			{
				auto [begin, it, end] = IterPolicy::Get(str, RangePolicy{}, until);
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
				-> decltype(ModifyPostProc::Transform(str.begin(), str.end(), IterPolicy{}))
			{
				auto [_, it, end] = IterPolicy::Get(str, RangePolicy{}, count);

				return ModifyPostProc::Transform(it, end, IterPolicy{}, &CType<value_type>::ToUpper);
			}

			// return type is void, in the case of in_place mode.
			constexpr void operator()(owner_type* pstr, ptrdiff_t count = detail::default_search_len) const noexcept
			{
				// explictly create a view, such that compiler won't complain about const& object expiring.
				view_type const str{ *pstr };
				auto [_, it, end] = IterPolicy::Get(str, RangePolicy{}, count);

				static_assert(
					typeid(ModifyPostProc::Transform(it, end, IterPolicy{}, &CType<value_type>::ToUpper)) == typeid(typename ctype_info::owner_type),
					"Upr() method must be used with marshaled returning types."
				);

				*pstr = ModifyPostProc::Transform(it, end, IterPolicy{}, &CType<value_type>::ToUpper);
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
		using namespace String::Functors::Components;

		using Str	= Utils<>;
		using StrI	= Utils<char,		Iterating::as_normal_ptr_t,	Comparing::case_ignored_t>;
		using StrR	= Utils<char,		Iterating::as_normal_ptr_t,	Comparing::regular_t,		Direction::backwards_t>;
		using StrIR	= Utils<char,		Iterating::as_normal_ptr_t,	Comparing::case_ignored_t,	Direction::backwards_t>;

		using Wcs	= Utils<wchar_t>;
		using WcsI	= Utils<wchar_t,	Iterating::as_normal_ptr_t,	Comparing::case_ignored_t>;
		using WcsR	= Utils<wchar_t,	Iterating::as_normal_ptr_t,	Comparing::regular_t,		Direction::backwards_t>;
		using WcsIR	= Utils<wchar_t,	Iterating::as_normal_ptr_t,	Comparing::case_ignored_t,	Direction::backwards_t>;

		using Mbs	= Utils<char,		Iterating::as_multibytes_t>;
		using MbsI	= Utils<char,		Iterating::as_multibytes_t,	Comparing::case_ignored_t>;
		using MbsR	= Utils<char,		Iterating::as_multibytes_t,	Comparing::regular_t,		Direction::backwards_t>;
		using MbsIR	= Utils<char,		Iterating::as_multibytes_t,	Comparing::case_ignored_t,	Direction::backwards_t>;

		inline constexpr auto StrLen = Linker<
			empty_comp_t,
			alg_cnt,
			ret_as_signed,
			iter_default,
			dir_forward,
			info_u8
		>{};

		inline constexpr auto MbsCSpn = Linker<
			empty_comp_t,
			alg_pbrk,
			ret_as_position,
			iter_multibytes,
			cmp_default,
			dir_forward,
			info_u8
		>{};

		inline constexpr auto MbsSpn = Linker<
			empty_comp_t,
			alg_spnp,
			ret_as_position,
			iter_multibytes,
			cmp_default,
			dir_forward,
			info_u8
		>{};

		inline constexpr auto MbsRCSpn = Linker<
			empty_comp_t,
			alg_pbrk,
			ret_as_position,
			iter_multibytes,
			cmp_default,
			dir_backward,
			info_u8
		>{};

		inline constexpr auto MbsRSpn = Linker<
			empty_comp_t,
			alg_spnp,
			ret_as_position,
			iter_multibytes,
			cmp_default,
			dir_backward,
			info_u8
		>{};
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

	using detail::strutl_decl::StrLen;

	using detail::strutl_decl::MbsCSpn;
	using detail::strutl_decl::MbsSpn;
	using detail::strutl_decl::MbsRCSpn;
	using detail::strutl_decl::MbsRSpn;
}

#ifdef GENERATOR_TY
#undef GENERATOR_TY
#endif



template <typename C>
[[nodiscard]]
constexpr auto UTIL_Trim(std::basic_string_view<C> sv) noexcept -> decltype(sv)
{
	using namespace Hydrogenium;
	using CT = CType<C>;

	auto ret =
		sv
		| std::views::drop_while(CT::IsSpace)
		| std::views::reverse
		| std::views::drop_while(CT::IsSpace)
		| std::views::reverse;

	if (std::ranges::empty(ret))
		return { sv.end(), sv.end(), };

	return {
		ret.begin().base().base(),
		ret.end().base().base(),
	};
}

// #TODO Str::ReplaceAll maybe??
template <typename C>
constexpr void UTIL_ReplaceAll(std::basic_string<C>* psz, std::basic_string_view<C> const& from, std::basic_string_view<C> const& to) noexcept
{
	if (from.empty())
		return;

	std::size_t start_pos = 0;
	while ((start_pos = psz->find(from, start_pos)) != psz->npos)
	{
		psz->replace(start_pos, from.length(), to);
		start_pos += to.length();	// In case 'to' contains 'from', like replacing 'x' with 'yx'
	}
}

// UTIL_Split