#pragma once

#include <algorithm>
#include <optional>
#include <ranges>
#include <string_view>



namespace Hydrogenium
{
	enum struct CodePoint : uint_fast8_t
	{
		WHOLE = 0,
		BEGIN_OF_2 = 2,
		BEGIN_OF_3,
		BEGIN_OF_4,
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
	};

	static_assert(CType<char>::CodePointOf(u8"A"[0]) == CodePoint::WHOLE);
	static_assert(CType<char>::CodePointOf(u8"Á"[0]) == CodePoint::BEGIN_OF_2);
	static_assert(CType<char>::CodePointOf(u8"あ"[0]) == CodePoint::BEGIN_OF_3);
	static_assert(CType<char>::CodePointOf(u8"あ"[1]) == CodePoint::MID);
	static_assert(CType<char>::CodePointOf(u8"𐒰"[0]) == CodePoint::BEGIN_OF_4);

	static_assert(CType<wchar_t>::CodePointOf(L"A"[0]) == CodePoint::WHOLE);
	static_assert(CType<wchar_t>::CodePointOf(L"Á"[0]) == CodePoint::WHOLE);
	static_assert(CType<wchar_t>::CodePointOf(L"あ"[0]) == CodePoint::WHOLE);
	static_assert(CType<wchar_t>::CodePointOf(L"𐒰"[0]) == CodePoint::BEGIN_OF_2);
	static_assert(CType<wchar_t>::CodePointOf(L"𐒰"[1]) == CodePoint::MID);
}

namespace Hydrogenium::StringPolicy::Advancing
{
	// #UPDATE_AT_CPP23 static operator()

	enum struct APRES : std::uint_fast8_t
	{
		ADVANCED = 0,
		RECEDED,

		BOS,	// begin of string
		EOS,	// end of string
		NOP,	// no-operation
		BAD_MB_POINT,
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

		static constexpr auto Dereference(auto&& iter) noexcept -> decltype(*iter)
		{
			return *iter;
		}

		static constexpr APRES Advance(auto& iter, auto&& end) noexcept
		{
			if (iter < end)
				++iter;
			else
				return APRES::EOS;

			return APRES::ADVANCED;
		}

		static constexpr APRES Recede(auto& iter, auto&& begin) noexcept
		{
			if (iter > begin)
				--iter;
			else
				return APRES::BOS;

			return APRES::RECEDED;
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

				return iter == end ? APRES::EOS : APRES::ADVANCED;
			}
			else if (num < 0)
			{
				while (num < 0 && iter > begin)
				{
					--iter;
					++num;
				}

				return iter == begin ? APRES::BOS : APRES::RECEDED;
			}
			else
				return APRES::NOP;

			std::unreachable();
		}

		static constexpr APRES FastForward(auto& iter, auto&& end, ptrdiff_t num) noexcept
		{
			if (num <= 0)
				return APRES::NOP;

			while (num > 0 && iter < end)
			{
				++iter;
				--num;
			}

			return iter == end ? APRES::EOS : APRES::ADVANCED;
		}

		static constexpr APRES Rewind(auto& iter, auto&& begin, ptrdiff_t num) noexcept
		{
			if (num <= 0)
				return APRES::NOP;

			while (num > 0 && iter > begin)
			{
				--iter;
				--num;
			}

			return iter == begin ? APRES::BOS : APRES::RECEDED;
		}
	};

	consteval bool UnitTest_as_normal_ptr_FWD() noexcept
	{
		std::string_view words{ "0123456789" };
		auto const bgn = words.begin(), ed = words.end();
		auto it = bgn + 3;

		if (as_normal_ptr::Dereference(it) != '3')
			return false;

		as_normal_ptr::FastForward(it, ed, 5);
		if (as_normal_ptr::Dereference(it) != '8')
			return false;

		as_normal_ptr::Rewind(it, bgn, 2);
		if (as_normal_ptr::Dereference(it) != '6')
			return false;

		as_normal_ptr::FastForward(it, ed, 100);
		as_normal_ptr::Advance(it, ed);
		as_normal_ptr::Arithmetic(it, bgn, ed, 100);
		if (it != ed)
			return false;

		as_normal_ptr::Rewind(it, bgn, 100);
		as_normal_ptr::Recede(it, bgn);
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

		if (as_normal_ptr::Dereference(it) != '3')
			return false;

		as_normal_ptr::FastForward(it, ed, 5);
		if (as_normal_ptr::Dereference(it) != '8')
			return false;

		as_normal_ptr::Rewind(it, bgn, 2);
		if (as_normal_ptr::Dereference(it) != '6')
			return false;

		as_normal_ptr::FastForward(it, ed, 100);
		as_normal_ptr::Advance(it, ed);
		as_normal_ptr::Arithmetic(it, bgn, ed, 100);
		if (it != ed)
			return false;

		as_normal_ptr::Rewind(it, bgn, 100);
		as_normal_ptr::Recede(it, bgn);
		as_normal_ptr::Arithmetic(it, bgn, ed, -100);
		if (it != bgn)
			return false;

		return true;
	}
	static_assert(UnitTest_as_normal_ptr_FWD());
	static_assert(UnitTest_as_normal_ptr_BWD());

	inline constexpr auto as_regular_ptr = as_normal_ptr{};

	struct as_utf8_t final
	{
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

		static constexpr APRES MoveToFirstCodePoint(auto& iter, auto&& begin, auto&& end) noexcept
		{
			using CT = CType<decltype(*iter)>;

			/*
			Consider this case:

			┌──'Á'──┐ ┌───────'𐒰'───────┐ ┌────'あ'────┐
			0xC3 0x81 0xF0 0x90 0x92 0xB0 0xE3 0x81 0x82
						   |
						   Your iterator.

			One should move this iterator to the position of 0xF0, it then would be reasonable to move to any other point.
			*/

			if (iter == begin)
			{
				auto const cp = CT::CodePointOf(*iter);

				if (cp == CodePoint::INVALID || cp == CodePoint::MID)
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

		// End of not required functions

		static constexpr auto Dereference(auto&& iter) noexcept -> decltype(*iter)
		{
			return *iter;
		}

		static constexpr auto ValueOf(auto&& iter) noexcept
		{
			using CT = CType<decltype(*iter)>;
			using view_type = typename CT::view_type;

			switch (CT::CodePointOf(*iter))
			{
			case CodePoint::WHOLE:
				return view_type{ iter, iter + 1 };

			case CodePoint::BEGIN_OF_2:
				return view_type{ iter, iter + 2 };

			case CodePoint::BEGIN_OF_3:
				return view_type{ iter, iter + 3 };

			case CodePoint::BEGIN_OF_4:
				return view_type{ iter, iter + 4 };

			default:
				std::abort();
			}

			std::unreachable();
		}

		static constexpr APRES Advance(auto& iter, auto&& end) noexcept
		{
			if (iter >= end)
				return APRES::EOS;

			using CT = CType<decltype(*iter)>;

			/*
			Consider this case:

			┌──'Á'──┐ ┌───────'𐒰'───────┐ ┌────'あ'────┐
			0xC3 0x81 0xF0 0x90 0x92 0xB0 0xE3 0x81 0x82
						   |
						   Your iterator.

			One should move this iterator to the position of 0xF0, then it would be reasonable to 'advance' to 0xE3.
			*/

			while (CT::CodePointOf(*iter) == CodePoint::MID)
				--iter;

			if (iter < end)
			{
				switch (CT::CodePointOf(*iter))
				{
				case CodePoint::WHOLE:
					++iter;
					break;

				case CodePoint::BEGIN_OF_2:
					iter += 2;
					break;

				case CodePoint::BEGIN_OF_3:
					iter += 3;
					break;

				case CodePoint::BEGIN_OF_4:
					iter += 4;
					break;

				default:
					std::abort();
				}
			}

			if (iter > end) [[unlikely]]
			{
				iter = end;
				return APRES::BAD_MB_POINT;
			}

			return iter == end ? APRES::EOS : APRES::ADVANCED;
		}

		static constexpr APRES Recede(auto& iter, auto&& begin) noexcept
		{
			using CT = CType<decltype(*iter)>;

			/*
			Consider this case:

			┌──'Á'──┐ ┌───────'𐒰'───────┐ ┌────'あ'────┐
			0xC3 0x81 0xF0 0x90 0x92 0xB0 0xE3 0x81 0x82
			               |
			               Your iterator.

			One should move this iterator to the position of 0xF0, then it would be reasonable to 'advance' to 0xC3.
			*/

			/*
			// Unfortunately it is not possible to impl this in receding process, as we have no info regarding endpos.
			// This could cause dereferencing end iterator.
			while (CT::CodePointOf(*iter) == CodePoint::MID)
				--iter;
			*/

			if (iter > begin)
			{
				do 
				{
					--iter;
				} while (iter > begin && CT::CodePointOf(*iter) == CodePoint::MID);
			}

			return iter == begin ? APRES::BOS : APRES::RECEDED;
		}

		static constexpr APRES Arithmetic(auto& iter, auto&& begin, auto&& end, ptrdiff_t num) noexcept
		{
			if (num > 0)
			{
				return FastForward(iter, end, num);
			}
			else if (num < 0)
			{
				return Rewind(iter, begin, -num);
			}
			else
				return APRES::NOP;

			std::unreachable();
		}

		static constexpr APRES FastForward(auto& iter, auto&& end, ptrdiff_t num) noexcept
		{
			if (num <= 0)
				return APRES::NOP;

			for (; num > 0; --num)
			{
				if (Advance(iter, end) == APRES::EOS)
					return APRES::EOS;
			}

			return APRES::ADVANCED;
		}

		static constexpr APRES Rewind(auto& iter, auto&& begin, ptrdiff_t num) noexcept
		{
			if (num <= 0)
				return APRES::NOP;

			for (; num > 0; --num)
			{
				if (Recede(iter, begin) == APRES::BOS)
					return APRES::BOS;
			}

			return APRES::RECEDED;
		}
	};

	constexpr bool UnitTest_as_multibytes_t_FWD() noexcept
	{
		std::string_view words{ u8"零一二三四五六七八九" };
		auto const bgn = words.begin(), ed = words.end();
		auto it = bgn + 3 * 3;

		if (as_utf8_t::ValueOf(it) != "三")
			return false;

		as_utf8_t::FastForward(it, ed, 5);
		if (as_utf8_t::ValueOf(it) != "八")
			return false;

		as_utf8_t::Rewind(it, bgn, 2);
		if (as_utf8_t::ValueOf(it) != "六")
			return false;

		as_utf8_t::FastForward(it, ed, 100);
		as_utf8_t::Advance(it, ed);
		as_utf8_t::Arithmetic(it, bgn, ed, 100);
		if (it != ed)
			return false;

		as_utf8_t::Rewind(it, bgn, 100);
		as_utf8_t::Recede(it, bgn);
		as_utf8_t::Arithmetic(it, bgn, ed, -100);
		if (it != bgn)
			return false;

		return true;
	}
	//static_assert(UnitTest_as_multibytes_t_FWD());

	inline constexpr auto as_utf8 = as_utf8_t{};
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
	// #CONTINUE_FROM_HERE

	// normal
	// backward
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
	concept AdvancingPolicy = requires (C* p, C const* cp)
	{
		{ T{}(p) } -> std::same_as<Hydrogenium::StringPolicy::Advancing::APRES>;
		{ T{}(cp) } -> std::same_as<Hydrogenium::StringPolicy::Advancing::APRES>;
		requires !requires{ { T{}(std::declval<C*>()) }; };	// must not be able to handle xvalue or rvalue, as this is treat as if lvalue increment.
	};

	static_assert(AdvancingPolicy<StringPolicy::Advancing::as_normal_ptr, char>);
	static_assert(AdvancingPolicy<StringPolicy::Advancing::as_utf8_t, unsigned char>);
	static_assert(!AdvancingPolicy<StringPolicy::Advancing::as_utf8_t, wchar_t>);

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
		String::AdvancingPolicy<char_type> auto Advancer = StringPolicy::Advancing::as_regular_ptr,
		String::ComparingPolicy<char_type> auto Comparators = StringPolicy::Comparing::regular,
		String::CounterPolicy<char_type> auto counter_fn = StringPolicy::Counter::cap_at_len
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
				auto it = str.cbegin();
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
				auto s1 = lhs.cbegin(), s2 = rhs.cbegin();
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
				auto it = str.cbegin();
				auto const end = it + std::min(str.size(), count);

				while (it < end)
				{
					// watch out for the N version.
					while (Advancer(it) == StringPolicy::Advancing::APRES::BAD_MB_POINT && it < end) {}
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

namespace Hydrogenium::String::UnitTest
{
	using namespace StringPolicy;

	using Str = Utils<>;
	using StrI = Utils<char, Advancing::as_regular_ptr, Comparing::case_ignored>;
	using StrN = Utils<char, Advancing::as_regular_ptr, Comparing::regular, Counter::cap_at_n>;
	using StrNI = Utils<char, Advancing::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n>;

	static_assert(StrI::Cmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrI::Cmp("abc", "DEF") < 0 && Str::Cmp("abc", "DEF") > 0);
	static_assert(StrI::Cmp("GHI", "def") > 0 && Str::Cmp("GHI", "def") < 0);
	static_assert(Str::Cmp(u8"你好", u8"你好") == 0 && Str::Cmp(u8"你好", u8"你好嗎") < 0);

	static_assert(Str::Chr("Try not", 't') == "t" && StrI::Chr("Try not", 'T') == "Try not");
	static_assert(StrN::Chr("Try not", 't', 4).empty() && StrNI::Chr("Try not", 't', 4) == "Try ");	// #UNDONE this is not good. the return of StrNI series should kept the original length.

	using Wcs = Utils<wchar_t>;
	using WcsI = Utils<wchar_t, Advancing::as_regular_ptr, Comparing::case_ignored>;
	using WcsN = Utils<wchar_t, Advancing::as_regular_ptr, Comparing::regular, Counter::cap_at_n>;
	using WcsNI = Utils<wchar_t, Advancing::as_regular_ptr, Comparing::case_ignored, Counter::cap_at_n>;

	static_assert(WcsI::Cmp(L"a0b1c2", L"A0B1C2") == 0);
	static_assert(WcsI::Cmp(L"abc", L"DEF") < 0 && Wcs::Cmp(L"abc", L"DEF") > 0);
	static_assert(WcsI::Cmp(L"GHI", L"def") > 0 && Wcs::Cmp(L"GHI", L"def") < 0);
	static_assert(Wcs::Cmp(L"你好", L"你好") == 0 && Wcs::Cmp(L"你好", L"你好嗎") < 0);
	static_assert(WcsN::Cmp(L"你好", L"你好嗎", 2) == 0 && WcsN::Cmp(L"你好", L"你好嗎", 3) < 0);

	static_assert(Wcs::Chr(L"Try not", L't') == L"t" && WcsI::Chr(L"Try not", L'T') == L"Try not");
	static_assert(WcsN::Chr(L"Try not", L't', 4).empty() && WcsNI::Chr(L"Try not", L't', 4) == L"Try ");

	using Mbs = Utils<unsigned char, Advancing::as_utf8>;
	using MbsN = Utils<unsigned char, Advancing::as_utf8, Comparing::regular, Counter::cap_at_n>;

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