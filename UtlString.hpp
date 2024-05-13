#pragma once

#include <cassert>
#include <cctype>
#include <cstdint>
#include <cwctype>

#include <algorithm>
#include <concepts>
#include <functional>
#include <limits>
#include <ranges>
#include <string_view>
#include <utility>



namespace Hydrogenium::UnitTest
{
	inline constexpr std::string_view ENG_TEXT_FWD = "0123456789";
	inline constexpr std::string_view ENG_TEXT_BWD = "9876543210";
	inline constexpr std::string_view CHN_TEXT_FWD = u8"零一二三四五六七八九";
	inline constexpr std::string_view CHN_TEXT_BWD = u8"九八七六五四三二一零";
	inline constexpr std::wstring_view RMN_WTEXT_FWD = L"ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩ";
	inline constexpr std::wstring_view RMN_WTEXT_BWD = L"ⅩⅨⅧⅦⅥⅤⅣⅢⅡⅠ";

	inline constexpr char32_t ENG_WORDS_FWD[] = { U'0', U'1', U'2', U'3', U'4', U'5', U'6', U'7', U'8', U'9', };
	inline constexpr char32_t ENG_WORDS_BWD[] = { U'9', U'8', U'7', U'6', U'5', U'4', U'3', U'2', U'1', U'0', };
	inline constexpr char32_t CHN_WORDS_FWD[] = { U'零', U'一', U'二', U'三', U'四', U'五', U'六', U'七', U'八', U'九', };
	inline constexpr char32_t CHN_WORDS_BWD[] = { U'九', U'八', U'七', U'六', U'五', U'四', U'三', U'二', U'一', U'零', };
	inline constexpr char32_t RMN_WORDS_FWD[] = { U'Ⅰ', U'Ⅱ', U'Ⅲ', U'Ⅳ', U'Ⅴ', U'Ⅵ', U'Ⅶ', U'Ⅷ', U'Ⅸ', U'Ⅹ' };
	inline constexpr char32_t RMN_WORDS_BWD[] = { U'Ⅹ', U'Ⅸ', U'Ⅷ', U'Ⅶ', U'Ⅵ', U'Ⅴ', U'Ⅳ', U'Ⅲ', U'Ⅱ', U'Ⅰ' };
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
		}

		//int toupper(int c);
		//std::wint_t towupper( std::wint_t ch );
		[[nodiscard]] static constexpr auto ToUpper(param_type c) noexcept -> decltype(c)
		{
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
		[[nodiscard]] static constexpr auto ToMultiBytes(fchar_t wc) noexcept -> multibytes_type requires (is_utf8 || is_utf16)
		{
			multibytes_type ret{};

			if constexpr (is_utf8)
			{
				if (wc <= 0x7Fu)
				{
					ret[0] = static_cast<param_type>(wc);
				}
				else if (wc <= 0x7FFu)
				{
					ret[0] = static_cast<param_type>(0xC0u | (wc >> 6));			/* 110xxxxx */
					ret[1] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */
				}
				else if (wc <= 0xFFFFu) {
					ret[0] = static_cast<param_type>(0xE0u | (wc >> 12));			/* 1110xxxx */
					ret[1] = static_cast<param_type>(0x80u | ((wc >> 6) & 0x3Fu));	/* 10xxxxxx */
					ret[2] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */
				}
				else if (wc <= 0x10FFFFu) {
					ret[0] = static_cast<param_type>(0xF0u | (wc >> 18));			/* 11110xxx */
					ret[1] = static_cast<param_type>(0x80u | ((wc >> 12) & 0x3Fu));	/* 10xxxxxx */
					ret[2] = static_cast<param_type>(0x80u | ((wc >> 6) & 0x3Fu));	/* 10xxxxxx */
					ret[3] = static_cast<param_type>(0x80u | (wc & 0x3Fu));			/* 10xxxxxx */
				}
			}
			else if constexpr (is_utf16)
			{
				if (wc < 0x10000u)
					ret[0] = static_cast<param_type>(wc);
				else if (wc <= 0x10FFFFu)
				{
					ret[0] = static_cast<param_type>(((((uint32_t)wc - 0x10000u) << 12) >> 22) + 0xD800u);
					ret[1] = static_cast<param_type>(((((uint32_t)wc - 0x10000u) << 22) >> 22) + 0xDC00u);
				}
			}
			else
			{
				assert(false);
			}

			return ret;
		}
	};

	// Unit testing at: UnitTest_UtlString_CType.cpp
}

namespace Hydrogenium::StringPolicy
{
	namespace Iterating
	{
		// Forward declearation.
		enum struct APRES : std::uint_fast8_t;
	}

	template <typename T, typename C>
	concept IteratingPolicy = requires (C * p, C const* cp, C* const pc)
	{
		T::normal_pointer;	// enables random-access-iter optimization.
		{ T::Initialize(p, pc, pc) } -> std::same_as<Iterating::APRES>;
		{ T::ValueOf(cp) } -> NonVoid;
		{ T::Arithmetic(p, pc, pc, ptrdiff_t{}) } -> std::same_as<Iterating::APRES>;

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

	struct MyDummy
	{
		static constexpr auto Impl(auto&&...) noexcept {}
		constexpr auto operator() (auto&&... args) const noexcept { return Impl(std::forward<decltype(args)>(args)...); }
	};

	template <typename T, typename C>
	concept CounterPolicy = requires
	{
		&T::template pattern_view_view<MyDummy, C>::operator();
		&T::template pattern_view_char<MyDummy, C>::operator();
		&T::template pattern_nullable_view<MyDummy, C>::operator();
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

		static constexpr APRES Initialize(auto&, auto&&...) noexcept
		{
			return APRES::NOP;
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

		static constexpr auto ArithCpy(auto iter, auto&& begin, auto&& end, ptrdiff_t num) noexcept -> decltype(iter)
		{
			Arithmetic(iter, begin, end, num);
			return iter;
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
					do
					{
						++iter;
					} while (iter < end && CT::CodePointOf(*iter) >= CodePoint::MID);

					if (iter >= end)
					{
						iter = end;
						return APRES::EOS;
					}

					return APRES::ADVANCED;
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

						return APRES::RECEDED;
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
		};
		// End of not required functions

		// Not matter what direction it is, this function always put iter onto the head of UTF sequence.
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
					do
					{
						--iter;

						if (auto const cp = CT::CodePointOf(*iter); cp <= CodePoint::BEGIN_OF_4)
							break;

					} while (iter > begin);

					return APRES::RECEDED;
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
					do
					{
						--iter;

						if (auto const cp = CT::CodePointOf(*iter); cp <= CodePoint::BEGIN_OF_4)
							break;

					} while (iter > begin);

					return APRES::RECEDED;
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

		static constexpr auto ArithCpy(auto iter, auto&& begin, auto&& end, ptrdiff_t num) noexcept -> decltype(iter)
		{
			Arithmetic(iter, begin, end, num);
			return iter;
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
				if (CType<T>::IsUpper(lhs))
					lhs = CType<T>::ToLower(lhs);
				if (CType<U>::IsUpper(rhs))
					rhs = CType<U>::ToLower(rhs);
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

namespace Hydrogenium::StringPolicy::Counter
{
	struct cap_at_n_t final
	{
		template <typename T, typename C>
		struct pattern_view_view
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type lhs, CType<C>::view_type rhs, ptrdiff_t count) const noexcept
			{
				return T::Impl(lhs, rhs, count);
			}
		};

		template <typename T, typename C>
		struct pattern_view_char
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str, CType<C>::param_type ch, ptrdiff_t last_pos) const noexcept
			{
				return T::Impl(str, ch, last_pos);
			}
		};

		template <typename T, typename C>
		struct pattern_nullable_view
		{
			[[nodiscard]]
			constexpr auto operator() (std::optional<typename CType<C>::view_type> str, CType<C>::view_type token, ptrdiff_t count) const noexcept
			{
				decltype(str) opt{ std::nullopt };

				if (str)
					opt.emplace(*str | std::views::take(count));

				return T::Impl(opt, token);
			}
		};

		template <typename T, typename C>
		struct pattern_view
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str, ptrdiff_t count) const noexcept
			{
				return T::Impl(str, count);
			}
		};
	};

	inline constexpr auto cap_at_n = cap_at_n_t{};

	struct cap_at_len_t final
	{
		template <typename T, typename C>
		struct pattern_view_view
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type lhs, CType<C>::view_type rhs) const noexcept
			{
				return T::Impl(lhs, rhs, std::numeric_limits<ptrdiff_t>::max());
			}
		};

		template <typename T, typename C>
		struct pattern_view_char
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str, CType<C>::param_type ch) const noexcept
			{
				return T::Impl(str, ch, std::numeric_limits<ptrdiff_t>::max());
			}
		};

		template <typename T, typename C>
		struct pattern_nullable_view
		{
			[[nodiscard]]
			constexpr auto operator() (std::optional<typename CType<C>::view_type> str, CType<C>::view_type token) const noexcept
			{
				return T::Impl(str, token);
			}
		};

		template <typename T, typename C>
		struct pattern_view
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str) const noexcept
			{
				return T::Impl(str, std::numeric_limits<ptrdiff_t>::max());
			}
		};
	};

	inline constexpr auto cap_at_len = cap_at_len_t{};

	static_assert(CounterPolicy<StringPolicy::Counter::cap_at_len_t, char>);
	static_assert(CounterPolicy<StringPolicy::Counter::cap_at_n_t, wchar_t>);
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
	struct as_it_is_t final
	{
		// Cmp
		constexpr auto operator() (std::same_as<int> auto val) const noexcept -> decltype(val)
		{
			return val;
		}

		// Len, Cnt, Spn, CSpn
		constexpr auto operator() (std::same_as<size_t> auto val) const noexcept -> decltype(val)
		{
			return val;
		}

		// Chr, PBrk, Str, Tok
		template <typename C>
		constexpr auto operator() (std::basic_string_view<C, std::char_traits<C>> val) const noexcept -> decltype(val)
		{
			return val;
		}

		// Dup, Lwr, Rev, Upr
		template <typename C>
		constexpr auto operator() (std::basic_string<C, std::char_traits<C>, std::allocator<C>>* ptr) const noexcept -> decltype(ptr)
		{
			return ptr;
		}
	};

	// Spn, CSpn(PBrk), Chr, Tok
	struct as_pointer_t final
	{
		[[nodiscard]]
		constexpr auto operator() (auto&& /*src*/, auto&& view, auto&& /*IterPolicy*/) const noexcept -> std::remove_cvref_t<decltype(view)>::const_pointer
		{
			if (view.empty())
				return nullptr;

			return std::addressof(view.front());
		}
	};
	struct as_position_t final
	{
		[[nodiscard]]
		constexpr auto operator() (auto&& src, auto&& view, auto IterPolicy) const noexcept -> std::ptrdiff_t
		{
			if (view.empty())
				return -1;

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
	};
	struct as_view_t final
	{
		[[nodiscard]]
		constexpr auto operator() (auto&& /*src*/, auto&& view, auto&& /*IterPolicy*/) const noexcept -> std::remove_cvref_t<decltype(view)>
		{
			return view;
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

	// Dup, Lwr, Rev, Upr
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
	};

	template <typename Q, typename T, typename C, typename M>
	struct postprocessor_t final
	{
		inline static constexpr Q Query = Q{};
		inline static constexpr T Test = T{};
		inline static constexpr C Counting = C{};
		inline static constexpr M Modify = M{};
	};

	inline constexpr auto as_it_is = postprocessor_t<as_view_t, as_lexic_t, as_unsigned_t, as_marshaled_t>{};
	static_assert(ResultProcessor<decltype(as_it_is), char>);

	inline constexpr auto to_c_style = postprocessor_t<as_pointer_t, as_lexic_t, as_unsigned_t, as_unmanaged_t>{};
	static_assert(ResultProcessor<decltype(to_c_style), char>);
}

namespace Hydrogenium::String
{
	template <
		typename char_type = char,
		StringPolicy::IteratingPolicy<char_type> auto IterPolicy = StringPolicy::Iterating::as_regular_ptr,
		StringPolicy::ComparingPolicy<char_type> auto Comparator = StringPolicy::Comparing::regular,
		StringPolicy::CounterPolicy<char_type> auto InvkPolicy = StringPolicy::Counter::cap_at_len,
		StringPolicy::DirectionPolicy<char_type> auto Range = StringPolicy::Direction::front_to_back,
		StringPolicy::ResultProcessor<char_type> auto RsltProc = StringPolicy::Result::as_it_is
	>
	struct Utils final
	{
		using ctype_info = CType<char_type>;
		using invoking_policy_t = std::remove_cvref_t<decltype(InvkPolicy)>;

		struct detail final
		{
			// Chr - v, c -> string_view
			__forceinline static constexpr auto Chr(ctype_info::view_type const& str, ctype_info::param_type ch, ptrdiff_t until) noexcept -> ctype_info::view_type
			{
				auto it = Range.Begin(str);
				auto const begin = Range.Begin(str),
					end = IterPolicy.ArithCpy(begin, begin, Range.End(str), std::min(std::ranges::ssize(str), until));

				for (; it < end; IterPolicy.Arithmetic(it, begin, end, 1))
				{
					if (Comparator.Eql(IterPolicy.ValueOf(it), ch))
						break;
				}

				if (it == end)
					return { str.end(), str.end() };

				if constexpr (Range.is_reverse)
				{
					return { ToForwardIter(it), ToForwardIter(begin, false) };
				}
				else
				{
					return { it, end };
				}
			}

			// Cmp - v, v -> int
			__forceinline static constexpr int Cmp(ctype_info::view_type const& lhs, ctype_info::view_type const& rhs, ptrdiff_t count) noexcept
			{
				auto s1 = Range.Begin(lhs), s2 = Range.Begin(rhs);
				auto const b1 = Range.Begin(lhs), b2 = Range.Begin(rhs);
				auto const e1 = IterPolicy.ArithCpy(b1, b1, Range.End(lhs), std::min(std::ranges::ssize(lhs), count)),
					e2 = IterPolicy.ArithCpy(b2, b2, Range.End(rhs), std::min(std::ranges::ssize(rhs), count));

				IterPolicy.Initialize(s1, b1, e1);
				IterPolicy.Initialize(s2, b2, e2);	// we are using while loop - ptr gets deref before being used.

				while (
					s1 < e1 && s2 < e2
					&& Comparator.Eql(IterPolicy.ValueOf(s1), IterPolicy.ValueOf(s2))
					)
				{
					IterPolicy.Arithmetic(s1, b1, e1, 1);
					IterPolicy.Arithmetic(s2, b2, e2, 1);
				}

				using value_type = decltype(IterPolicy.ValueOf(s1));

				// Preventing deducing as something like 'int32_t'
				value_type const c1 = s1 == e1 ? '\0' : IterPolicy.ValueOf(s1);
				value_type const c2 = s2 == e2 ? '\0' : IterPolicy.ValueOf(s2);

				return Comparator.Cmp(c1, c2);
			}

			// Cnt - v -> size_t; Counting graphemes in a char[] range.
			__forceinline static constexpr size_t Cnt(ctype_info::view_type const& str, ptrdiff_t count) noexcept
			{
				auto it = Range.Begin(str);
				auto const begin = Range.Begin(str),
					end = IterPolicy.ArithCpy(begin, begin, Range.End(str), std::min(std::ranges::ssize(str), count));

				size_t n = begin == end ? 0 : 1;	// #UPDATE_AT_CPP23 size type literal
				for (; IterPolicy.Arithmetic(it, begin, end, 1) & StringPolicy::Iterating::APRES::MOVED; ++n) {}

				return n;
			}

			// CSpn - v, v -> size_t; Use PBrk instead.

			// Dup - v -> string
			// Len - v -> size_t
			// Lwr - o -> string

			// PBrk - v, v -> string_view; served as CSpn, Spn, SpnP as well.
			__forceinline static constexpr auto PBrk(ctype_info::view_type const& dest, ctype_info::view_type const& src, ptrdiff_t count, bool bSpnPMode) noexcept -> ctype_info::view_type
			{
				// the src is NOT order-sensitive, it's no more than a library of what to search.
				// hence no range function put onto src.

				auto s1 = Range.Begin(dest);
				auto const b1 = Range.Begin(dest),
					e1 = IterPolicy.ArithCpy(b1, b1, Range.End(dest), std::min(std::ranges::ssize(dest), count));

				auto s2 = src.begin();
				auto const b2 = src.begin(),
					e2 = IterPolicy.ArithCpy(b2, b2, src.end(), src.size());

				IterPolicy.Initialize(s1, b1, e1);
				IterPolicy.Initialize(s2, b2, e2);	// we are using while loop - ptr gets deref before being used.

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

				if (s1 == e1)
					return { dest.end(), dest.end() };

				if constexpr (Range.is_reverse)
				{
					return { ToForwardIter(s1), ToForwardIter(b1, false) };
				}
				else
				{
					return { s1, e1 };
				}
			}

			// Rev - o -> string
			// Spn - v, v -> size_t; Use PBrk instead.
			// SpnP - v, v -> string_view; Use PBrk instead.
			// Str - v, v -> string_view
			// Tok - n, v -> string_view
			// Upr - o -> string
		};

#pragma region Chr
		struct chr_fn_t : invoking_policy_t::template pattern_view_char<chr_fn_t, char_type>
		{
			using super = invoking_policy_t::template pattern_view_char<chr_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& str, ctype_info::param_type ch, ptrdiff_t until) noexcept
			{
				return RsltProc.Query(
					str,
					detail::Chr(str, ch, until),
					IterPolicy
				);
			}
		};
		static inline constexpr auto Chr = chr_fn_t{};
#pragma endregion Chr

#pragma region Cmp
		struct cmp_fn_t : invoking_policy_t::template pattern_view_view<cmp_fn_t, char_type>
		{
			using super = invoking_policy_t::template pattern_view_view<cmp_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& lhs, ctype_info::view_type const& rhs, ptrdiff_t count) noexcept
			{
				return RsltProc.Test(
					detail::Cmp(lhs, rhs, count)
				);
			}
		};
		static inline constexpr auto Cmp = cmp_fn_t{};
#pragma endregion Cmp

#pragma region Cnt
		struct cnt_fn_t : invoking_policy_t::template pattern_view<cnt_fn_t, char_type>
		{
			using super = invoking_policy_t::template pattern_view<cnt_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& str, ptrdiff_t count) noexcept
			{
				return RsltProc.Counting(
					detail::Cnt(str, count)
				);
			}
		};
		static inline constexpr auto Cnt = cnt_fn_t{};
#pragma endregion Cnt

#pragma region Dup
		struct dup_fn_t : invoking_policy_t::template pattern_view<dup_fn_t, char_type>
		{
			using super = invoking_policy_t::template pattern_view<dup_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& str, ptrdiff_t count) noexcept
			{
				return RsltProc.Modify(
					str | std::views::take(count)
				);
			}
		};
		static inline constexpr auto Dup = dup_fn_t{};
#pragma endregion Dup

#pragma region PBrk
		struct pbrk_fn_t : invoking_policy_t::template pattern_view_view<pbrk_fn_t, char_type>
		{
			using super = invoking_policy_t::template pattern_view_view<pbrk_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& dest, ctype_info::view_type const& src, ptrdiff_t count) noexcept
			{
				return RsltProc.Query(
					dest,
					detail::PBrk(dest, src, count, false),
					IterPolicy
				);
			}
		};
		static inline constexpr auto PBrk = pbrk_fn_t{};
#pragma endregion PBrk

#pragma region SpnP
		struct spnp_fn_t : invoking_policy_t::template pattern_view_view<spnp_fn_t, char_type>
		{
			using super = invoking_policy_t::template pattern_view_view<spnp_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& dest, ctype_info::view_type const& src, ptrdiff_t count) noexcept
			{
				return RsltProc.Query(
					dest,
					detail::PBrk(dest, src, count, true),
					IterPolicy
				);
			}
		};
		static inline constexpr auto SpnP = spnp_fn_t{};
#pragma endregion SpnP
	};
}

namespace Hydrogenium
{
	namespace detail::strutl_decl
	{
		using namespace String;
		using namespace StringPolicy;

		using Str		= Utils<>;
		using StrI		= Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored>;
		using StrN		= Utils<char, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n>;
		using StrNI		= Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n>;
		using StrR		= Utils<char, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_len, Direction::back_to_front>;
		using StrIR		= Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_len, Direction::back_to_front>;
		using StrNR		= Utils<char, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n, Direction::back_to_front>;
		using StrNIR	= Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n, Direction::back_to_front>;

		using Wcs = Utils<wchar_t>;
		using WcsI = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::case_ignored>;
		using WcsN = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n>;
		using WcsNI = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n>;
		using WcsR = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_len, Direction::back_to_front>;
		using WcsIR = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_len, Direction::back_to_front>;
		using WcsNR = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n, Direction::back_to_front>;
		using WcsNIR = Utils<wchar_t, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n, Direction::back_to_front>;

		using Mbs = Utils<char, Iterating::as_multibytes>;
		using MbsI = Utils<char, Iterating::as_multibytes, Comparing::case_ignored>;
		using MbsN = Utils<char, Iterating::as_multibytes, Comparing::regular, Counter::cap_at_n>;
		using MbsNI = Utils<char, Iterating::as_multibytes, Comparing::case_ignored, Counter::cap_at_n>;
		using MbsR = Utils<char, Iterating::as_multibytes, Comparing::regular, Counter::cap_at_len, Direction::back_to_front>;
		using MbsIR = Utils<char, Iterating::as_multibytes, Comparing::case_ignored, Counter::cap_at_len, Direction::back_to_front>;
		using MbsNR = Utils<char, Iterating::as_multibytes, Comparing::regular, Counter::cap_at_n, Direction::back_to_front>;
		using MbsNIR = Utils<char, Iterating::as_multibytes, Comparing::case_ignored, Counter::cap_at_n, Direction::back_to_front>;
	}

	using detail::strutl_decl::Str;
	using detail::strutl_decl::StrI;
	using detail::strutl_decl::StrN;
	using detail::strutl_decl::StrNI;
	using detail::strutl_decl::StrR;
	using detail::strutl_decl::StrIR;
	using detail::strutl_decl::StrNR;
	using detail::strutl_decl::StrNIR;

	using detail::strutl_decl::Wcs;
	using detail::strutl_decl::WcsI;
	using detail::strutl_decl::WcsN;
	using detail::strutl_decl::WcsNI;
	using detail::strutl_decl::WcsR;
	using detail::strutl_decl::WcsIR;
	using detail::strutl_decl::WcsNR;
	using detail::strutl_decl::WcsNIR;

	using detail::strutl_decl::Mbs;
	using detail::strutl_decl::MbsI;
	using detail::strutl_decl::MbsN;
	using detail::strutl_decl::MbsNI;
	using detail::strutl_decl::MbsR;
	using detail::strutl_decl::MbsIR;
	using detail::strutl_decl::MbsNR;
	using detail::strutl_decl::MbsNIR;
}



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