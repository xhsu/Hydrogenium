#pragma once

#include <algorithm>
#include <optional>
#include <ranges>
#include <string_view>



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
	constexpr auto ToForwardIter(auto iter) noexcept
	{
		if constexpr (MultiWrappedRevIter<decltype(iter)>)
		{
			return ToForwardIter(iter.base());
		}
		else if constexpr (ReverseIterator<decltype(iter)>)
		{
			// &*(reverse_iterator(i)) == &*(i - 1)
			return (++iter).base();
		}
		else
		{
			return iter;
		}
	}

	// Pointing to same elem!! Recursively remove all wrapped r_iter<>
	constexpr auto ToReverseIter(auto iter) noexcept
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
			// &*(reverse_iterator(i)) == &*(i - 1)
			return std::make_reverse_iterator(iter) - 1;
		}
	}

	namespace UnitTest
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

	template <typename T>
	struct CType final
	{
		using char_type = std::remove_cvref_t<T>;
		static inline constexpr bool is_narrow = std::is_same_v<char_type, char> || std::is_same_v<char_type, signed char> || std::is_same_v<char_type, unsigned char>;
		static inline constexpr bool is_wide = std::is_same_v<char_type, wchar_t>;

		using param_type = std::conditional_t<is_narrow, unsigned char, wchar_t>;
		using eof_type = std::common_type_t<decltype(EOF), decltype(WEOF)>;
		using view_type = std::conditional_t<is_narrow, ::std::string_view, ::std::wstring_view>;
		using owner_type = std::conditional_t<is_narrow, ::std::string, ::std::wstring>;
		using traits_type = ::std::char_traits<char_type>;
		using multibytes_type = std::conditional_t<is_narrow, std::array<param_type, 4>, std::array<param_type, 2>>;

		static_assert(is_narrow || is_wide, "T must be one of char, signed char, unsigned char or wchar_t.");

		static inline constexpr eof_type eof = is_narrow ? EOF : WEOF;

		//int isalnum(int c);
		//int iswalnum( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsAlNum(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
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
				else
					return std::iswalnum(c);
			}
		}

		//int isalpha(int c);
		//int iswalpha( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsAlpha(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return
					('a' <= c && c <= 'z')
					|| ('A' <= c && c <= 'Z');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isalpha(c);
				else
					return std::iswalpha(c);
			}
		}

		//int isblank(int c);
		//int iswblank( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsBlank(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return c == '\t' || c == ' ';
			}
			else
			{
				if constexpr (is_narrow)
					return std::isblank(c);
				else
					return std::iswblank(c);
			}
		}

		//int iscntrl(int c);
		//int iswcntrl( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsCntrl(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return
					('\x00' <= c && c <= '\x1F')
					|| c == '\x7F';
			}
			else
			{
				if constexpr (is_narrow)
					return std::iscntrl(c);
				else
					return std::iswcntrl(c);
			}
		}

		//int isdigit(int c);
		//int iswdigit( wint_t ch );
		[[nodiscard]] static constexpr bool IsDigit(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return ('0' <= c && c <= '9');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isdigit(c);
				else
					return std::iswdigit(c);
			}
		}

		//int isgraph(int c);
		//int iswgraph( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsGraph(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return ('\x21' <= c && c <= '\x7E');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isgraph(c);
				else
					return std::iswgraph(c);
			}
		}

		//int islower(int c);
		//int iswlower( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsLower(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return 'a' <= c && c <= 'z';
			}
			else
			{
				if constexpr (is_narrow)
					return std::islower(c);
				else
					return std::iswlower(c);
			}
		}

		//int isprint(int c);
		//int iswprint(std::wint_t ch);
		[[nodiscard]] static constexpr bool IsPrint(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return ('\x20' <= c && c <= '\x7E');
			}
			else
			{
				if constexpr (is_narrow)
					return std::isprint(c);
				else
					return std::iswprint(c);
			}
		}

		//int ispunct(int c);
		//int iswpunct( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsPunct(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
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
				else
					return std::iswpunct(c);
			}
		}

		//int isspace(int c);
		//int iswspace( wint_t ch );
		[[nodiscard]] static constexpr bool IsSpace(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
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
				else
					return std::iswspace(c);
			}
		}

		//int isupper(int c);
		//int iswupper( std::wint_t ch );
		[[nodiscard]] static constexpr bool IsUpper(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
			{
				return 'A' <= c && c <= 'Z';
			}
			else
			{
				if constexpr (is_narrow)
					return std::isupper(c);
				else
					return std::iswupper(c);
			}
		}

		//int isxdigit(int c);
		//int iswxdigit( wint_t ch );
		[[nodiscard]] static constexpr bool IsXDigit(param_type c) noexcept
		{
			if (std::is_constant_evaluated())
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
				else
					return std::iswxdigit(c);
			}
		}

		//int tolower(int c);
		//std::wint_t towlower( std::wint_t ch );
		[[nodiscard]] static constexpr auto ToLower(param_type c) noexcept -> decltype(c)
		{
			if (std::is_constant_evaluated())
			{
				if ('A' <= c && c <= 'Z')
					return static_cast<decltype(c)>(c - 'A' + 'a');

				return c;
			}
			else
			{
				if constexpr (is_narrow)
					return static_cast<decltype(c)>(std::tolower(c));
				else
					return static_cast<decltype(c)>(std::towlower(c));
			}
		}

		//int toupper(int c);
		//std::wint_t towupper( std::wint_t ch );
		[[nodiscard]] static constexpr auto ToUpper(param_type c) noexcept -> decltype(c)
		{
			if (std::is_constant_evaluated())
			{
				if ('a' <= c && c <= 'z')
					return static_cast<decltype(c)>(c - 'a' + 'A');

				return c;
			}
			else
			{
				if constexpr (is_narrow)
					return static_cast<decltype(c)>(std::toupper(c));
				else
					return static_cast<decltype(c)>(std::towupper(c));
			}

		}

		// Luna's extension
		[[nodiscard]] static constexpr auto CodePointOf(param_type c) noexcept -> CodePoint
		{
			auto const u = static_cast<uint32_t>(c);

			// UTF-8
			if constexpr (sizeof(c) == sizeof(unsigned char))
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
			else if constexpr (sizeof(c) == sizeof(char16_t))
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
			else if constexpr (sizeof(c) == sizeof(char32_t))
			{
				return CodePoint::WHOLE;
			}
		}

		// Luna's extension
		[[nodiscard]] static constexpr auto ToFullWidth(view_type bytes) noexcept -> char32_t
		{
			constexpr bool is_utf8 = sizeof(bytes[0]) == sizeof(uint8_t);
			constexpr bool is_utf16 = sizeof(bytes[0]) == sizeof(char16_t);

			switch (CodePointOf(bytes.front()))
			{
			case CodePoint::WHOLE:
				return static_cast<char32_t>(bytes.front());

			case CodePoint::BEGIN_OF_2:
			{
				assert(bytes.size() == 2);

				if constexpr (is_utf8)
				{
					char32_t ret = (bytes[0] & 0b00011111) << 6 | (bytes[1] & 0b00111111);

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
					char32_t ret = (bytes[0] & 0b00001111) << 12 | (bytes[1] & 0b00111111) << 6 | (bytes[2] & 0b00111111);

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
					char32_t ret =
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
		[[nodiscard]] static constexpr auto ToMultiBytes(char32_t wc) noexcept -> multibytes_type
		{
			multibytes_type ret{};

			if constexpr (is_narrow)
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
			else if constexpr (is_wide)
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

	namespace UnitTest
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

		static_assert(CType<wchar_t>::CodePointOf(L"A"[0]) == CodePoint::WHOLE);
		static_assert(CType<wchar_t>::CodePointOf(L"Á"[0]) == CodePoint::WHOLE);
		static_assert(CType<wchar_t>::CodePointOf(L"あ"[0]) == CodePoint::WHOLE);
		static_assert(CType<wchar_t>::CodePointOf(L"𐒰"[0]) == CodePoint::BEGIN_OF_2);
		static_assert(CType<wchar_t>::CodePointOf(L"𐒰"[1]) == CodePoint::MID);

		static_assert(CType<wchar_t>::ToFullWidth(L"A") == U'A');
		static_assert(CType<wchar_t>::ToFullWidth(L"Á") == U'Á');
		static_assert(CType<wchar_t>::ToFullWidth(L"あ") == U'あ');
		static_assert(CType<wchar_t>::ToFullWidth(L"𐒰") == U'𐒰');

		using U16MBARR = std::array<wchar_t, 2>;
		static_assert(CType<wchar_t>::ToMultiBytes(U'A') == U16MBARR{ L'A', 0 });
		static_assert(CType<wchar_t>::ToMultiBytes(U'Á') == U16MBARR{ L'Á', 0 });
		static_assert(CType<wchar_t>::ToMultiBytes(U'あ') == U16MBARR{ L'あ', 0 });
		static_assert(CType<wchar_t>::ToMultiBytes(U'𐒰') == U16MBARR{ 0xD801, 0xDCB0 });
	}
}

namespace Hydrogenium::StringPolicy::Iterating
{
	// #UPDATE_AT_CPP23 static operator()

	enum struct APRES : std::uint_fast8_t
	{
		ADVANCED = (1 << 0),
		RECEDED = (1 << 1),

		BOS = (1 << 2),	// begin of string
		EOS = (1 << 3),	// end of string
		NOP = (1 << 4),	// no-operation
		BAD_MB_POINT = (1 << 5),
	};

	struct as_normal_ptr final
	{
		constexpr APRES operator() (auto& ptr) const noexcept
		{
			auto& c = *ptr;

			if (c == '\0')
				return APRES::EOS;

			++ptr;
			return APRES::ADVANCED;
		}

		static constexpr APRES Initialize(...) noexcept
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
				while (num > 0 && iter < end)
				{
					++iter;
					--num;
				}

				return iter >= end ? APRES::EOS : APRES::ADVANCED;
			}
			else if (num < 0)
			{
				while (num < 0 && iter > begin)
				{
					--iter;
					++num;
				}

				return iter <= begin ? APRES::BOS : APRES::RECEDED;
			}
			else
				return APRES::NOP;

			std::unreachable();
		}
	};

	consteval bool UnitTest_as_normal_ptr_FWD() noexcept
	{
		std::string_view words{ "0123456789" };
		auto const bgn = words.begin(), ed = words.end();
		auto it = bgn + 3;

		static_assert(std::is_same_v<decltype(as_normal_ptr::ValueOf(bgn)), unsigned char>);

		if (as_normal_ptr::ValueOf(it) != '3')
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, 5);
		if (as_normal_ptr::ValueOf(it) != '8')
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, -2);
		if (as_normal_ptr::ValueOf(it) != '6')
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, 100);
		if (it != ed)
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, -100);
		if (it != bgn)
			return false;

		return true;
	}
	consteval bool UnitTest_as_normal_ptr_BWD() noexcept
	{
		std::string_view words{ "9876543210" };
		auto const bgn = words.rbegin(), ed = words.rend();
		auto it = bgn + 3;

		if (as_normal_ptr::ValueOf(it) != '3')
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, 5);
		if (as_normal_ptr::ValueOf(it) != '8')
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, -2);
		if (as_normal_ptr::ValueOf(it) != '6')
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, 100);
		if (it != ed)
			return false;

		as_normal_ptr::Arithmetic(it, bgn, ed, -100);
		if (it != bgn)
			return false;

		return true;
	}
	static_assert(UnitTest_as_normal_ptr_FWD());
	static_assert(UnitTest_as_normal_ptr_BWD());

	inline constexpr auto as_regular_ptr = as_normal_ptr{};

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

		constexpr APRES operator() (auto& ptr) const noexcept
			requires (sizeof(decltype(ptr[0])) == sizeof(unsigned char))
		{
			// UTF-8 is designed for 8-bits, so this is a forced cast, no generic involved.
			auto c = static_cast<unsigned char>(*ptr);

			if (c == '\0')
				return APRES::EOS;

			if (c <= 0x7F)
				++ptr;
			else if ((c & 0b111'000'00) == 0b110'000'00)
				ptr += 2;
			else if ((c & 0b1111'0000) == 0b1110'0000)
				ptr += 3;
			else if ((c & 0b11111'000) == 0b11110'000)
				ptr += 4;
			else if ((c & 0b11'000000) == 0b10'000000)
			{
				++ptr;	// broken UTF8, this ptr is pointing to somewhere in the middle of an multibyte string.
				return APRES::BAD_MB_POINT;
			}
			else
				std::unreachable();

			return APRES::ADVANCED;
		}

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

		static constexpr auto ValueOf(auto&& iter) noexcept -> char32_t	// #TODO UTF-8 view type, char[4]-based.
		{
			using CT = CType<decltype(*iter)>;
			using view_type = typename CT::view_type;	// #MSVC_BUGGED_tailing_return_type_namespace_error

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
			using CT = CType<decltype(*iter)>;

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

			if (iter == end || iter == begin)
				Initialize(iter, begin, end);

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
	};

	inline constexpr auto as_multibytes = as_multibytes_t{};
}

namespace Hydrogenium::StringPolicy::Comparing
{
	struct case_ignored_t final
	{
		template <typename T, typename U> [[nodiscard]]
		static constexpr int Cmp(T lhs, U rhs) noexcept
		{
			if (CType<T>::IsUpper(lhs))
				lhs = CType<T>::ToLower(lhs);
			if (CType<U>::IsUpper(rhs))
				rhs = CType<U>::ToLower(rhs);

			return lhs - rhs;
		}

		template <typename T, typename U> [[nodiscard]]
		static constexpr bool Eql(T lhs, U rhs) noexcept
		{
			if (CType<T>::IsUpper(lhs))
				lhs = CType<T>::ToLower(lhs);
			if (CType<U>::IsUpper(rhs))
				rhs = CType<U>::ToLower(rhs);

			return lhs == rhs;
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
}

namespace Hydrogenium::StringPolicy::Counter
{
	struct cap_at_n_t final
	{
		template <typename T, typename C>
		struct pattern_view_view
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type lhs, CType<C>::view_type rhs, size_t count) const noexcept
			{
				return T::Impl(lhs, rhs, count);
			}
		};

		template <typename T, typename C>
		struct pattern_view_char
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str, CType<C>::param_type ch, size_t last_pos) const noexcept
			{
				return T::Impl(str, ch, last_pos);
			}
		};

		template <typename T, typename C>
		struct pattern_nullable_view
		{
			[[nodiscard]]
			constexpr auto operator() (std::optional<typename CType<C>::view_type> str, CType<C>::view_type token, size_t count) const noexcept
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
			constexpr auto operator() (CType<C>::view_type str, size_t count) const noexcept
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
				return T::Impl(lhs, rhs, CType<C>::view_type::npos);
			}
		};

		template <typename T, typename C>
		struct pattern_view_char
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str, CType<C>::param_type ch) const noexcept
			{
				return T::Impl(str, ch, CType<C>::view_type::npos);
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
				return T::Impl(str, CType<C>::view_type::npos);
			}
		};
	};

	inline constexpr auto cap_at_len = cap_at_len_t{};
}

namespace Hydrogenium::StringPolicy::Direction
{
	struct forwards final
	{
		static constexpr auto Begin(auto&& r) noexcept
		{
			return std::ranges::begin(r);
		}

		static constexpr auto End(auto&& r) noexcept
		{
			return std::ranges::end(r);
		}
	};

	struct backwards final
	{
		static constexpr auto Begin(auto&& r) noexcept
		{
			return std::ranges::rbegin(r);
		}

		static constexpr auto End(auto&& r) noexcept
		{
			return std::ranges::rend(r);
		}
	};
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
}

namespace Hydrogenium::String
{
	template <typename T, typename C>
	concept IteratingPolicy = requires (C* p, C const* cp)
	{
		{ T{}(p) } -> std::same_as<Hydrogenium::StringPolicy::Iterating::APRES>;
		{ T{}(cp) } -> std::same_as<Hydrogenium::StringPolicy::Iterating::APRES>;
		requires !requires{ { T{}(std::declval<C*>()) }; };	// must not be able to handle xvalue or rvalue, as this is treat as if lvalue increment.
	};

	static_assert(IteratingPolicy<StringPolicy::Iterating::as_normal_ptr, char>);
	static_assert(IteratingPolicy<StringPolicy::Iterating::as_multibytes_t, unsigned char>);
	static_assert(!IteratingPolicy<StringPolicy::Iterating::as_multibytes_t, wchar_t>);

	template <typename T, typename C>
	concept ComparingPolicy = requires (C c)
	{
		{ T{}.Cmp(c, c) } -> std::same_as<int>;
		{ T{}.Eql(c, c) } -> std::same_as<bool>;
	};

	static_assert(ComparingPolicy<StringPolicy::Comparing::case_ignored_t, char>);
	static_assert(ComparingPolicy<StringPolicy::Comparing::regular_t, char>);

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

	static_assert(CounterPolicy<StringPolicy::Counter::cap_at_len_t, char>);
	static_assert(CounterPolicy<StringPolicy::Counter::cap_at_n_t, wchar_t>);

	template <typename T, typename C>
	concept DirectionPolicy = requires (CType<C>::view_type view)
	{
		{ T::Begin(view) } -> std::bidirectional_iterator;
		{ T::End(view) } -> std::bidirectional_iterator;
		{ T::Begin(view) } -> std::input_iterator;
		{ T::End(view) } -> std::input_iterator;
	};

	static_assert(DirectionPolicy<StringPolicy::Direction::forwards, char>);
	static_assert(DirectionPolicy<StringPolicy::Direction::backwards, char>);

	template <typename T, typename C>
	concept ResultPolicy = requires (T t, CType<C>::view_type v, CType<C>::owner_type *p)
	{
		std::invoke(t, (int)0);
		std::invoke(t, (size_t)0);
		std::invoke(t, v);
		std::invoke(t, p);
	};

	static_assert(ResultPolicy<StringPolicy::Result::as_it_is_t, char>);
}

namespace Hydrogenium::String
{
	template <
		typename char_type = char,
		String::IteratingPolicy<char_type> auto Advancer = StringPolicy::Iterating::as_regular_ptr,
		String::ComparingPolicy<char_type> auto Comparators = StringPolicy::Comparing::regular,
		String::CounterPolicy<char_type> auto counter_fn = StringPolicy::Counter::cap_at_len,
		String::DirectionPolicy<char_type> auto Range = StringPolicy::Direction::forwards{}
	>
	struct Utils final
	{
		using ctype_info = CType<char_type>;

		using advancer_t = decltype(Advancer);
		using comparators_t = decltype(Comparators);
		using call_sigs_t = std::remove_cvref_t<decltype(counter_fn)>;

		struct detail final
		{
			// Chr - v, c -> string_view
			__forceinline static constexpr auto Chr(ctype_info::view_type const& str, ctype_info::param_type ch, size_t until) noexcept -> ctype_info::view_type
			{
				auto it = Range.Begin(str);
				auto const end = it + std::min(str.size(), until);

				for (; it < end; Advancer(it))
				{
					if (Comparators.Eql(*it, ch))
						break;
				}

				return { it, end };
			}

			// Cmp - v, v -> int
			__forceinline static constexpr int Cmp(ctype_info::view_type const& lhs, ctype_info::view_type const& rhs, size_t count) noexcept
			{
				auto s1 = Range.Begin(lhs), s2 = Range.Begin(rhs);
				auto const e1 = s1 + std::min(lhs.size(), count), e2 = s2 + std::min(rhs.size(), count);

				while (
					s1 < e1 && s2 < e2
					&& Comparators.Eql(*s1, *s2)
					)
				{
					Advancer(s1);
					Advancer(s2);
				}

				typename ctype_info::param_type const c1 = s1 == e1 ? '\0' : *s1;
				typename ctype_info::param_type const c2 = s2 == e2 ? '\0' : *s2;

				return Comparators.Cmp(c1, c2);
			}

			// Cnt - v -> size_t; Counting graphemes in a char[] range.
			__forceinline static constexpr size_t Cnt(ctype_info::view_type const& str, size_t count) noexcept
			{
				size_t n{};
				auto it = Range.Begin(str);
				auto const end = it + std::min(str.size(), count);

				while (it < end)
				{
					// watch out for the N version.
					while (Advancer(it) == StringPolicy::Iterating::APRES::BAD_MB_POINT && it < end) {}
					++n;
				}

				return n;
			}

			// CSpn - v, v -> size_t
			__forceinline constexpr size_t CSpn(ctype_info::view_type const& dest, ctype_info::view_type const& src) noexcept
			{
				if (auto const pos = dest.find_first_of(src); pos != dest.npos)
					return pos;

				return dest.length();
			}

			// Dup - v -> string
			__forceinline constexpr auto Dup(ctype_info::view_type const& str) noexcept -> ctype_info::owner_type
			{
				return typename ctype_info::owner_type{ str };
			}

			// Len - v -> size_t
			// Lwr - o -> string
			// PBrk - v, v -> string_view
			// Rev - o -> string
			// Spn - v, v -> size_t
			// Str - v, v -> string_view
			// Tok - n, v -> string_view
			// Upr - o -> string
		};

#pragma region Chr
		struct chr_fn_t : call_sigs_t::template pattern_view_char<chr_fn_t, char_type>
		{
			using super = call_sigs_t::template pattern_view_char<chr_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& str, ctype_info::param_type ch, size_t until) noexcept
			{
				return detail::Chr(str, ch, until);
			}
		};
		static inline constexpr auto Chr = chr_fn_t{};
#pragma endregion Chr

#pragma region Cmp
		struct cmp_fn_t : call_sigs_t::template pattern_view_view<cmp_fn_t, char_type>
		{
			using super = call_sigs_t::template pattern_view_view<cmp_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& lhs, ctype_info::view_type const& rhs, size_t count) noexcept
			{
				return detail::Cmp(lhs, rhs, count);
			}
		};
		static inline constexpr auto Cmp = cmp_fn_t{};
#pragma endregion Cmp

#pragma region Cnt
		struct cnt_fn_t : call_sigs_t::template pattern_view<cnt_fn_t, char_type>
		{
			using super = call_sigs_t::template pattern_view<cnt_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& str, size_t count) noexcept
			{
				return detail::Cnt(str, count);
			}
		};
		static inline constexpr auto Cnt = cnt_fn_t{};
#pragma endregion Cnt
	};
}

namespace Hydrogenium::StringPolicy::UnitTest
{
	using namespace Hydrogenium::UnitTest;

	template <typename Policy>
	constexpr bool UnitTest_iterating_policy(auto&& bgn, auto&& ed, std::span<char32_t const> results) noexcept
	{
		auto it = bgn;
		Policy::Initialize(it, bgn, ed);
		if (Policy::ValueOf(it) != results.front())
			return false;

		Policy::Arithmetic(it, bgn, ed, 3);
		if (Policy::ValueOf(it) != results[3])
			return false;

		Policy::Arithmetic(it, bgn, ed, 0);
		if (Policy::ValueOf(it) != results[3])
			return false;

		Policy::Arithmetic(it, bgn, ed, 100);
		if (it != ed)
			return false;

		Policy::Arithmetic(it, bgn, ed, -4);
		if (Policy::ValueOf(it) != results[5])
			return false;

		Policy::Arithmetic(it, bgn, ed, -100);
		if (Policy::ValueOf(it) != results[0])	// NOT it == bgn!! in the case of reverse UTF sequence, the iterator will be at bgn if and only if the char is ASCII.
			return false;

		return true;
	}

	static_assert(UnitTest_iterating_policy<StringPolicy::Iterating::as_multibytes_t>(CHN_TEXT_FWD.begin(), CHN_TEXT_FWD.end(), CHN_WORDS_FWD));
	static_assert(UnitTest_iterating_policy<StringPolicy::Iterating::as_multibytes_t>(CHN_TEXT_FWD.rbegin(), CHN_TEXT_FWD.rend(), CHN_WORDS_BWD));
	static_assert(UnitTest_iterating_policy<StringPolicy::Iterating::as_multibytes_t>(RMN_WTEXT_FWD.begin(), RMN_WTEXT_FWD.end(), RMN_WORDS_FWD));
	static_assert(UnitTest_iterating_policy<StringPolicy::Iterating::as_multibytes_t>(RMN_WTEXT_FWD.rbegin(), RMN_WTEXT_FWD.rend(), RMN_WORDS_BWD));
	static_assert(UnitTest_iterating_policy<StringPolicy::Iterating::as_multibytes_t>(ENG_TEXT_FWD.begin(), ENG_TEXT_FWD.end(), ENG_WORDS_FWD));
	static_assert(UnitTest_iterating_policy<StringPolicy::Iterating::as_multibytes_t>(ENG_TEXT_FWD.rbegin(), ENG_TEXT_FWD.rend(), ENG_WORDS_BWD));
}

namespace Hydrogenium::String::UnitTest
{
	using namespace StringPolicy;

	using Str = Utils<>;
	using StrI = Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored>;
	using StrN = Utils<char, Iterating::as_regular_ptr, Comparing::regular, Counter::cap_at_n>;
	using StrNI = Utils<char, Iterating::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n>;

	static_assert(StrI::Cmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrI::Cmp("abc", "DEF") < 0 && Str::Cmp("abc", "DEF") > 0);
	static_assert(StrI::Cmp("GHI", "def") > 0 && Str::Cmp("GHI", "def") < 0);
	static_assert(Str::Cmp(u8"你好", u8"你好") == 0 && Str::Cmp(u8"你好", u8"你好嗎") < 0);

	static_assert(Str::Chr("Try not", 't') == "t" && StrI::Chr("Try not", 'T') == "Try not");
	static_assert(StrN::Chr("Try not", 't', 4).empty() && StrNI::Chr("Try not", 't', 4) == "Try ");	// #UNDONE this is not good. the return of StrNI series should kept the original length.

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

	using Mbs = Utils<unsigned char, Iterating::as_multibytes>;
	using MbsN = Utils<unsigned char, Iterating::as_multibytes, Comparing::regular, Counter::cap_at_n>;

	static_assert(Mbs::Cnt(u8"Heraclius") == Str::Cnt(u8"Heraclius"));
	static_assert(Mbs::Cnt(u8"Ἡράκλειος") == 9);
	static_assert(Mbs::Cnt(u8"Héraclius") == 9);
	static_assert(Mbs::Cnt(u8"Ираклий") == 7);
	static_assert(Mbs::Cnt(u8"ヘラクレイオス") == 7);
	static_assert(Mbs::Cnt(u8"希拉克略") == 4);
	//static_assert(MbsN::Cnt(u8"Heráclio", 5) == 5); // #CONTINUE_FROM_HERE
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