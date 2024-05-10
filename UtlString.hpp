#pragma once

#include <algorithm>
#include <optional>
#include <ranges>
#include <string_view>



namespace Hydrogenium
{
	template <typename T>
	struct CType final
	{
		static inline constexpr bool is_narrow = std::is_same_v<T, char> || std::is_same_v<T, signed char> || std::is_same_v<T, unsigned char>;
		static inline constexpr bool is_wide = std::is_same_v<T, wchar_t>;

		using param_type = std::conditional_t<is_narrow, unsigned char, wchar_t>;
		using eof_type = std::common_type_t<decltype(EOF), decltype(WEOF)>;
		using view_type = std::conditional_t<is_narrow, ::std::string_view, ::std::wstring_view>;
		using owner_type = std::conditional_t<is_narrow, ::std::string, ::std::wstring>;
		using char_type = T;
		using traits_type = ::std::char_traits<T>;

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
	};
}

namespace Hydrogenium::StringPolicy::Advancing
{
	// #UPDATE_AT_CPP23 static operator()

	enum struct APRES : std::uint_fast8_t
	{
		ADVANCED = 0,
		EOS,	// end of string
		BAD_MB_POINT,
	};

	struct as_ascii_t final
	{
		constexpr APRES operator() (auto& ptr) const noexcept
		{
			auto& c = *ptr;

			if (c == '\0')
				return APRES::EOS;

			++ptr;
			return APRES::ADVANCED;
		}
	};

	inline constexpr auto as_ascii = as_ascii_t{};

	struct as_utf8_t final
	{
		constexpr APRES operator() (auto& ptr) const noexcept
		{
			auto& c = *ptr;

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
	};

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
		using size_type = size_t;

		template <typename T, typename C>
		struct pattern_view_view
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type lhs, CType<C>::view_type rhs, size_t count) const noexcept
			{
				return T::Impl(
					lhs | std::views::take(count),
					rhs | std::views::take(count)
				);
			}
		};

		template <typename T, typename C>
		struct pattern_view_char
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str, CType<C>::param_type ch, size_t last_pos) const noexcept
			{
				return T::Impl(
					str | std::views::take(last_pos),
					ch
				);
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
				return T::Impl(lhs, rhs);
			}
		};

		template <typename T, typename C>
		struct pattern_view_char
		{
			[[nodiscard]]
			constexpr auto operator() (CType<C>::view_type str, CType<C>::param_type ch) const noexcept
			{
				return T::Impl(str, ch);
			}
		};

		template <typename T, typename C>
		struct pattern_nullable_view
		{
			[[nodiscard]]
			constexpr auto operator() (std::optional<typename CType<C>::view_type> str, CType<C>::view_type token, size_t count) const noexcept
			{
				return T::Impl(str, token);
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
	// #CONTINUE_FROM_HERE
	// index
	// iterator
	// dup

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

	static_assert(AdvancingPolicy<StringPolicy::Advancing::as_ascii_t, char>);
	static_assert(AdvancingPolicy<StringPolicy::Advancing::as_utf8_t, unsigned char>);

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
}

namespace Hydrogenium::String
{
	template <
		typename char_type = char,
		String::AdvancingPolicy<char_type> auto Advancer = StringPolicy::Advancing::as_ascii,
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
			__forceinline static constexpr auto Chr(ctype_info::view_type const& str, ctype_info::param_type ch) noexcept -> ctype_info::view_type
			{
				auto it = str.cbegin();
				for (; it != str.cend(); ++it)
				{
					if (Comparators.Eql(*it, ch))
						break;
				}

				return { it, str.cend() };
			}

			// Cmp - v, v -> int
			__forceinline static constexpr int Cmp(ctype_info::view_type const& lhs, ctype_info::view_type const& rhs) noexcept
			{
				auto s1 = lhs.cbegin(), s2 = rhs.cbegin();
				auto const e1 = lhs.cend(), e2 = rhs.cend();

				while (
					s1 != e1 && s2 != e2
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

			// Cnt - v -> size_t
			// CSpn - v, v -> size_t
			// Dup - v -> string
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

			static constexpr auto Impl(ctype_info::view_type const& str, ctype_info::param_type ch) noexcept
			{
				return detail::Chr(str, ch);
			}
		};
		static inline constexpr auto Chr = chr_fn_t{};
#pragma endregion Chr

#pragma region Cmp
		struct cmp_fn_t : call_sigs_t::template pattern_view_view<cmp_fn_t, char_type>
		{
			using super = call_sigs_t::template pattern_view_view<cmp_fn_t, char_type>;
			using super::operator();

			static constexpr auto Impl(ctype_info::view_type const& lhs, ctype_info::view_type const& rhs) noexcept
			{
				return detail::Cmp(lhs, rhs);
			}
		};
		static inline constexpr auto Cmp = cmp_fn_t{};
#pragma endregion Cmp
	};
}

namespace Hydrogenium::String::UnitTest
{
	using namespace StringPolicy;

	using Str = Utils<>;
	using StrI = Utils<char, Advancing::as_ascii, Comparing::case_ignored>;
	using StrN = Utils<char, Advancing::as_ascii, Comparing::regular, StringPolicy::Counter::cap_at_n>;
	using StrNI = Utils<char, Advancing::as_ascii, Comparing::case_ignored, StringPolicy::Counter::cap_at_n>;

	static_assert(StrI::Cmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrI::Cmp("abc", "DEF") < 0 && Str::Cmp("abc", "DEF") > 0);
	static_assert(StrI::Cmp("GHI", "def") > 0 && Str::Cmp("GHI", "def") < 0);
	static_assert(Str::Cmp(u8"你好", u8"你好") == 0 && Str::Cmp(u8"你好", u8"你好嗎") < 0);

	static_assert(Str::Chr("Try not", 't') == "t" && StrI::Chr("Try not", 'T') == "Try not");
	static_assert(StrN::Chr("Try not", 't', 4).empty() && StrNI::Chr("Try not", 't', 4) == "Try ");	// #UNDONE this is not good. the return of StrNI series should kept the original length.

	using Wcs = Utils<wchar_t>;
	using WcsI = Utils<wchar_t, Advancing::as_ascii, Comparing::case_ignored>;
	using WcsN = Utils<wchar_t, Advancing::as_ascii, Comparing::regular, StringPolicy::Counter::cap_at_n>;
	using WcsNI = Utils<wchar_t, Advancing::as_ascii, Comparing::case_ignored, StringPolicy::Counter::cap_at_n>;

	static_assert(WcsI::Cmp(L"a0b1c2", L"A0B1C2") == 0);
	static_assert(WcsI::Cmp(L"abc", L"DEF") < 0 && Wcs::Cmp(L"abc", L"DEF") > 0);
	static_assert(WcsI::Cmp(L"GHI", L"def") > 0 && Wcs::Cmp(L"GHI", L"def") < 0);
	static_assert(Wcs::Cmp(L"你好", L"你好") == 0 && Wcs::Cmp(L"你好", L"你好嗎") < 0);
	static_assert(WcsN::Cmp(L"你好", L"你好嗎", 2) == 0 && WcsN::Cmp(L"你好", L"你好嗎", 3) < 0);

	static_assert(Wcs::Chr(L"Try not", L't') == L"t" && WcsI::Chr(L"Try not", L'T') == L"Try not");
	static_assert(WcsN::Chr(L"Try not", L't', 4).empty() && WcsNI::Chr(L"Try not", L't', 4) == L"Try ");
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