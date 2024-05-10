#pragma once

#include <algorithm>
#include <optional>
#include <ranges>
#include <string_view>



namespace Hydrogenium::CCType
{
	// #UPDATE_AT_CPP23 if consteval

	// int isalnum(int c);
	constexpr bool IsAlNum(unsigned char c) noexcept
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
			return std::isalnum(c);
		}
	}

	// int isalpha(int c);
	constexpr bool IsAlpha(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return
				('a' <= c && c <= 'z')
				|| ('A' <= c && c <= 'Z');
		}
		else
		{
			return std::isalpha(c);
		}
	}

	//int isblank(int c);
	constexpr bool IsBlank(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return c == '\t' || c == ' ';
		}
		else
		{
			return std::isblank(c);
		}
	}

	//int iscntrl(int c);
	constexpr bool IsCntrl(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return
				('\x00' <= c && c <= '\x1F')
				|| c == '\x7F';
		}
		else
		{
			return std::iscntrl(c);
		}
	}

	//int isdigit(int c);
	constexpr bool IsDigit(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return ('0' <= c && c <= '9');
		}
		else
		{
			return std::isdigit(c);
		}
	}

	//int isgraph(int c);
	constexpr bool IsGraph(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return ('\x21' <= c && c <= '\x7E');
		}
		else
		{
			return std::isgraph(c);
		}
	}

	//int islower(int c);
	constexpr bool IsLower(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return 'a' <= c && c <= 'z';
		}
		else
		{
			return std::islower(c);
		}
	}

	//int isprint(int c);
	constexpr bool IsPrint(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return ('\x20' <= c && c <= '\x7E');
		}
		else
		{
			return std::isprint(c);
		}
	}

	//int ispunct(int c);
	constexpr bool IsPunct(unsigned char c) noexcept
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
			return std::ispunct(c);
		}
	}

	//int isspace(int c);
	constexpr bool IsSpace(unsigned char c) noexcept
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
			return std::isspace(c);
		}
	}

	//int isupper(int c);
	constexpr bool IsUpper(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return 'A' <= c && c <= 'Z';
		}
		else
		{
			return std::isupper(c);
		}
	}

	//int isxdigit(int c);
	constexpr bool IsXDigit(unsigned char c) noexcept
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
			return std::isxdigit(c);
		}
	}

	//int tolower(int c);
	constexpr auto ToLower(unsigned char c) noexcept -> decltype(c)
	{
		if (std::is_constant_evaluated())
		{
			if ('A' <= c && c <= 'Z')
				return static_cast<decltype(c)>(c - 'A' + 'a');

			return c;
		}
		else
		{
			return static_cast<decltype(c)>(std::tolower(c));
		}
	}

	//int toupper(int c);
	constexpr auto ToUpper(unsigned char c) noexcept -> decltype(c)
	{
		if (std::is_constant_evaluated())
		{
			if ('a' <= c && c <= 'z')
				return static_cast<decltype(c)>(c - 'a' + 'A');

			return c;
		}
		else
		{
			return static_cast<decltype(c)>(std::toupper(c));
		}

	}
}

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

namespace Hydrogenium::CString
{
	using std::optional;
	using std::size_t;
	using std::string;
	using std::string_view;

// #UPDATE_AT_CPP23 if consteval

	//void* memcpy(void* s1, const void* s2, size_t n);      // freestanding
	//void* memmove(void* s1, const void* s2, size_t n);     // freestanding

	//char* strcpy(char* s1, const char* s2);                // freestanding
	constexpr string* StrCpy(string* dest, string_view src) noexcept
	{
		dest->assign(src);
		return dest;
	}

	//char* strncpy(char* s1, const char* s2, size_t n);     // freestanding
	constexpr string* StrNCpy(string* dest, string_view src, size_t count) noexcept
	{
		dest->assign(
			src.data(),
			std::min(src.length(), count)
		);

		// Not going to have UB or add additional '\0' to the end of dest.

		return dest;
	}

	//char* strcat(char* s1, const char* s2);                // freestanding
	constexpr string* StrCat(string* dest, string_view src) noexcept
	{
		dest->append(src);
		return dest;
	}

	//char* strncat(char* s1, const char* s2, size_t n);     // freestanding
	constexpr string* StrNCat(string* dest, string_view src, size_t count) noexcept
	{
		dest->append(
			src.data(),
			std::min(src.length(), count)
		);

		return dest;
	}

	//int memcmp(const void* s1, const void* s2, size_t n);  // freestanding

	//int strcmp(const char* s1, const char* s2);            // freestanding
	constexpr int StrCmp(string_view lhs, string_view rhs) noexcept
	{
		return string_view::traits_type::compare(
			lhs.data(),
			rhs.data(),
			std::min(lhs.length(), rhs.length())
		);
	}
	static_assert(StrCmp("Cats", "Cats") == 0);
	static_assert(StrCmp("Garfield", "Heathcliff") < 0);
	static_assert(StrCmp("Snagglepuss", "Hobbes") > 0);

	//int strcoll(const char* s1, const char* s2);

	//int strncmp(const char* s1, const char* s2, size_t n); // freestanding
	constexpr int StrNCmp(string_view lhs, string_view rhs, size_t count) noexcept
	{
		return string_view::traits_type::compare(
			lhs.data(),
			rhs.data(),

			// UB removed.
			std::ranges::min({ lhs.length(), rhs.length(), count })
		);
	}
	static_assert(StrNCmp("Hello, world!", "Hello, everybody!", 13) > 0);
	static_assert(StrNCmp("Hello, everybody!", "Hello, world!", 13) < 0);
	static_assert(StrNCmp("Hello, everybody!", "Hello, world!", 7) == 0);
	static_assert(StrNCmp("Hello, !", "Hello, !", 0x1000) == 0);

	//size_t strxfrm(char* s1, const char* s2, size_t n);
	//const void* memchr(const void* s, int c, size_t n);    // freestanding
	//void* memchr(void* s, int c, size_t n);                // freestanding

	//const char* strchr(const char* s, int c);              // freestanding
	//char* strchr(char* s, int c);                          // freestanding
	constexpr optional<string_view> StrChr(string_view str, unsigned char ch) noexcept
	{
		if (auto const pos = str.find_first_of(ch); pos != str.npos)
			return optional<string_view>{ std::in_place, str.substr(pos) };

		return std::nullopt;
	}
	static_assert(*StrChr("Try not", 'T') == "Try not");
	static_assert(!StrChr("Try not", 'N'));

	//size_t strcspn(const char* s1, const char* s2);        // freestanding
	constexpr size_t StrCSpn(string_view dest, string_view src) noexcept
	{
		if (auto const pos = dest.find_first_of(src); pos != string_view::npos)
			return pos;

		return dest.length();
	}
	static_assert(StrCSpn("abcde312$#@", "*$#") == 8);

	//const char* strpbrk(const char* s1, const char* s2);   // freestanding
	//char* strpbrk(char* s1, const char* s2);               // freestanding
	constexpr optional<string_view> StrPBrk(string_view dest, string_view breakset) noexcept
	{
		if (auto const pos = dest.find_first_of(breakset); pos != dest.npos)
			return optional<string_view>{ std::in_place, dest.substr(pos) };

		return std::nullopt;
	}
	static_assert(*StrPBrk("hello, world!", " ,!") == ", world!");
	static_assert(!StrPBrk("hello, world!", "@~").has_value());

	//const char* strrchr(const char* s, int c);             // freestanding
	//char* strrchr(char* s, int c);                         // freestanding
	constexpr optional<string_view> StrRChr(string_view str, unsigned char ch) noexcept
	{
		if (auto const pos = str.find_last_of(ch); pos != str.npos)
			return optional<string_view>{ std::in_place, str.substr(pos) };

		return std::nullopt;
	}
	static_assert(*StrRChr("/home/user/hello.c", '/') == "/hello.c");
	static_assert(!StrRChr("/home/user/hello.c", '\\').has_value());

	//size_t strspn(const char* s1, const char* s2);         // freestanding
	constexpr size_t StrSpn(string_view dest, string_view src) noexcept
	{
		if (auto const pos = dest.find_first_not_of(src); pos != string_view::npos)
			return pos;

		return dest.length();
	}
	static_assert(StrSpn("abcde312$#@", "qwertyuiopasdfghjklzxcvbnm") == 5);

	//const char* strstr(const char* s1, const char* s2);    // freestanding
	//char* strstr(char* s1, const char* s2);                // freestanding
	constexpr optional<string_view> StrStr(string_view haystack, string_view needle) noexcept
	{
		if (auto const pos = haystack.find(needle); pos != string_view::npos)
			return optional<string_view>{ std::in_place, haystack.substr(pos) };

		return std::nullopt;
	}
	static_assert(*StrStr("Try not. Do, or do not. There is no try.", "not") == "not. Do, or do not. There is no try.");
	static_assert(!StrStr("haystack", "needle"));

	//char* strtok(char* s1, const char* s2);                // freestanding
	optional<string_view> StrTok(optional<string_view> str, string_view delim)
	{
		static thread_local const char* buffer = nullptr, *last_excl = nullptr;

		if (str.has_value())
		{
			buffer = str->data();
			last_excl = buffer + str->size();
		}

		buffer += StrSpn({ buffer, last_excl }, delim);

		if (buffer >= last_excl)
			return std::nullopt;

		auto const tokenBegin = buffer;

		buffer += StrCSpn({ buffer, last_excl }, delim);

		if (buffer >= last_excl)
			return std::nullopt;

		// buffer is the end in this section as it is going to be assigned as '\0' in original strtok().
		return optional<string_view>{ std::in_place, tokenBegin, buffer };
	}

	void UnitTest_StrTok() noexcept
	{
		string_view input = "one + two * (three - four)!";
		string_view delimiters = "! +- (*)";

		auto token = StrTok(input, delimiters);
		while (token)
		{
			std::cout << '"' << *token << '"' << ' ';
			token = StrTok(std::nullopt, delimiters);
		}

		std::cout << "\nExpected output: " R"("one" "two" "three" "four")" << std::endl;
	}

	//void* memset(void* s, int c, size_t n);                // freestanding
	//char* strerror(int errnum);

	//size_t strlen(const char* s);                          // freestanding
	constexpr size_t StrLen(string_view str) noexcept
	{
		return str.length();
	}
	static_assert(StrLen("dog cat\0mouse") == 7);

	//char *strdup( const char *src );						// (since C23)
	constexpr string StrDup(string_view str) noexcept
	{
		return string{ str };
	}

	//char *strndup( const char *src, size_t size );		// (since C23)
	constexpr string StrNDup(string_view src, size_t size) noexcept
	{
		return string{ src.substr(size) };
	}

	namespace detail
	{
		constexpr bool CaseIgnoredEqual(char lhs, char rhs) noexcept
		{
			if (CCType::IsUpper(lhs))
				lhs = static_cast<decltype(lhs)>(lhs - 'A' + 'a');
			if (CCType::IsUpper(rhs))
				rhs = static_cast<decltype(rhs)>(rhs - 'A' + 'a');

			return lhs == rhs;
		}
	}

	//int _stricmp(const char* string1, const char* string2);	// MSVC
	constexpr int StrICmp(string_view lhs, string_view rhs) noexcept
	{
		auto s1 = lhs.cbegin(), s2 = rhs.cbegin();
		auto const e1 = lhs.cend(), e2 = rhs.cend();

		while (
			s1 != e1 && s2 != e2
			&& detail::CaseIgnoredEqual(*s1, *s2)
			)
		{
			++s1;
			++s2;
		}


		// treat as if it were NUL terminus if end() reached.
		// MS _stricmp() is treat as if all case are lower.
		// see test:
		// fmt::println("(abc) stricmp: {}; strcmp: {}", _stricmp("abc", "[\\]^_`"), strcmp("abc", "[\\]^_`"));
		// fmt::println("(ABC) stricmp: {}; strcmp: {}", _stricmp("ABC", "[\\]^_`"), strcmp("ABC", "[\\]^_`"));

		unsigned char const c1 = s1 == e1 ? '\0' : CCType::ToLower(*s1);
		unsigned char const c2 = s2 == e2 ? '\0' : CCType::ToLower(*s2);

		return c1 - c2;
	}
	static_assert(StrICmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrICmp("abc", "DEF") < 0 && StrCmp("abc", "DEF") > 0);
	static_assert(StrICmp("GHI", "def") > 0 && StrCmp("GHI", "def") < 0);
	static_assert(StrICmp(u8"你好", u8"你好") == 0 && StrICmp(u8"你好", u8"你好嗎") < 0);

	//char *_strlwr(char* str);								// MSVC
	constexpr string StrLwr(string_view str) noexcept
	{
		string ret{};
		ret.reserve(str.length());

		for (auto&& c : str)
			ret.push_back(CCType::ToLower(c));

		return ret;
	}
	constexpr string* StrLwr(string* str) noexcept
	{
		for (auto& c : *str)
			c = CCType::ToLower(c);

		return str;
	}
	static_assert(StrLwr(u8"AbCdEfG01234") == u8"abcdefg01234");	// UTF-8 Tested. MSVC is bugged in compile-time UTF.

	//int _strnicmp(const char* string1, const char* string2, size_t count);	// MSVC
	constexpr int StrNICmp(string_view lhs, string_view rhs, size_t count) noexcept
	{
		return StrICmp(
			lhs | std::views::take(count),
			rhs | std::views::take(count)
		);
	}
	static_assert(StrNICmp("", "", 0x100) == 0);
	static_assert(StrNICmp(u8"你好", u8"你好嗎", StrLen(u8"你好")) == 0);

	//size_t _strncnt(const char* str, size_t count);		// MSVC

	//unsigned int _strnextc(const char* str);				// MSVC

	//char *_strninc(const char* str, size_t count);		// MSVC

	//char *_strrev(char* str);								// MSVC
	constexpr string StrRev(string_view str) noexcept
	{
		return
			str
			| std::views::reverse
			| std::ranges::to<string>();
	}
	constexpr string* StrRev(string* str) noexcept
	{
		auto ret = StrRev(*str);
		std::swap(*str, ret);
		return str;
	}
	static_assert(StrRev("AbCdEfG01234") == "43210GfEdCbA");

	//char *_strspnp(const char* str, const char* charset);	// MSVC
	constexpr string_view StrSpnP(string_view str, string_view charset) noexcept
	{
		if (auto const pos = str.find_first_not_of(charset); pos != string_view::npos)
			return str.substr(pos);

		return string_view{ str.cend(), str.cend() };
	}
	static_assert(StrSpnP("cabbage", "c") == "abbage");
	static_assert(StrSpnP("haystack", "needle") == "haystack");
	static_assert(StrSpnP("01234", "0123456789").empty());

	//char *_strupr(char* str);								// MSVC
	constexpr string StrUpr(string_view str) noexcept
	{
		string ret{};
		ret.reserve(str.length());

		for (auto&& c : str)
			ret.push_back(CCType::ToUpper(c));

		return ret;
	}
	constexpr string* StrUpr(string* str) noexcept
	{
		for (auto& c : *str)
			c = CCType::ToUpper(c);

		return str;
	}
	static_assert(StrUpr(u8"AbCdEfG01234") == u8"ABCDEFG01234");	// UTF-8 Tested. MSVC is bugged in compile-time UTF.

	//char *stristr(char* str, const char* substr);			// Quake
	constexpr optional<string_view> StrIStr(string_view haystack, string_view needle) noexcept
	{
		optional<string_view> opt{ std::nullopt };

		auto ret = std::ranges::search(
			haystack,
			needle,
			{},
			CCType::ToLower,
			CCType::ToLower
		);

		if (ret.empty())
			return opt;

		auto const pos = std::ranges::distance(haystack.begin(), ret.begin());

		opt.emplace(haystack.substr(pos));
		return opt;
	}
	static_assert(*StrIStr("abCD abcd", "Bcd") == "bCD abcd");
	static_assert(!StrIStr("abcd abcd", ""));
	static_assert(!StrIStr("abcd abcd", "efg"));
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
		template <typename T> [[nodiscard]]
		static constexpr int Cmp(T lhs, T rhs) noexcept
		{
			if (CType<T>::IsUpper(lhs))
				lhs = CType<T>::ToLower(lhs);
			if (CType<T>::IsUpper(rhs))
				rhs = CType<T>::ToLower(rhs);

			return lhs - rhs;
		}

		template <typename T> [[nodiscard]]
		static constexpr bool Eql(T lhs, T rhs) noexcept
		{
			if (CType<T>::IsUpper(lhs))
				lhs = CType<T>::ToLower(lhs);
			if (CType<T>::IsUpper(rhs))
				rhs = CType<T>::ToLower(rhs);

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
	};

	inline constexpr auto cap_at_n = cap_at_n_t{};

	struct cap_at_len_t final
	{
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
}

namespace Hydrogenium::String
{
	template <
		typename char_type = char,
		String::AdvancingPolicy<char_type> auto adv_fn = StringPolicy::Advancing::as_ascii,
		String::ComparingPolicy<char_type> auto comp_fn = StringPolicy::Comparing::regular,
		auto counter_fn = StringPolicy::Counter::cap_at_len
	>
	struct Utils final
	{
		using ctype_info = CType<char_type>;

		using adv_fn_t = decltype(adv_fn);
		using comp_fn_t = decltype(comp_fn);
		using counter_fn_t = decltype(counter_fn);

		struct detail final
		{
			__forceinline static constexpr int Cmp(ctype_info::view_type const& lhs, ctype_info::view_type const& rhs) noexcept
			{
				auto s1 = lhs.cbegin(), s2 = rhs.cbegin();
				auto const e1 = lhs.cend(), e2 = rhs.cend();

				while (
					s1 != e1 && s2 != e2
					&& comp_fn.Eql(*s1, *s2)
					)
				{
					adv_fn(s1);
					adv_fn(s2);
				}

				typename ctype_info::param_type const c1 = s1 == e1 ? '\0' : *s1;
				typename ctype_info::param_type const c2 = s2 == e2 ? '\0' : *s2;

				return comp_fn.Cmp(c1, c2);
			}

			__forceinline static constexpr auto Chr(ctype_info::view_type const& str, ctype_info::param_type ch) noexcept -> ctype_info::view_type
			{
				auto it = str.cbegin();
				for (; it != str.cend(); ++it)
				{
					if (comp_fn.Eql(*it, ch))
						break;
				}

				return { it, str.cend() };
			}
		};

		[[nodiscard]]
		static constexpr int Cmp(ctype_info::view_type lhs, ctype_info::view_type rhs) noexcept
			requires (!requires{ typename counter_fn_t::size_type; })
		{
			return detail::Cmp(lhs, rhs);
		}

		[[nodiscard]]
		static constexpr int Cmp(ctype_info::view_type lhs, ctype_info::view_type rhs, size_t count) noexcept
			requires requires{ typename counter_fn_t::size_type; }
		{
			return detail::Cmp(
				lhs | std::views::take(count),
				rhs | std::views::take(count)
			);
		}

		[[nodiscard]]
		static constexpr auto Chr(ctype_info::view_type str, ctype_info::param_type ch) noexcept -> ctype_info::view_type
			requires (!requires{ typename counter_fn_t::size_type; })
		{
			return detail::Chr(str, ch);
		}

		[[nodiscard]]
		static constexpr auto Chr(ctype_info::view_type str, ctype_info::param_type ch, size_t count) noexcept -> ctype_info::view_type
			requires requires{ typename counter_fn_t::size_type; }
		{
			return detail::Chr(str.substr(count), ch);
		}
	};
}

namespace Hydrogenium::String::UnitTest
{
	using namespace CString;
	using namespace StringPolicy;

	using Str = Utils<>;
	using StrI = Utils<char, Advancing::as_ascii, Comparing::case_ignored>;
	using StrN = Utils<char, Advancing::as_ascii, Comparing::regular, StringPolicy::Counter::cap_at_n>;
	using StrNI = Utils<char, Advancing::as_ascii, Comparing::case_ignored, StringPolicy::Counter::cap_at_n>;

	static_assert(StrI::Cmp("a0b1c2", "A0B1C2") == 0);
	static_assert(StrI::Cmp("abc", "DEF") < 0 && Str::Cmp("abc", "DEF") > 0);
	static_assert(StrI::Cmp("GHI", "def") > 0 && Str::Cmp("GHI", "def") < 0);
	static_assert(Str::Cmp(u8"你好", u8"你好") == 0 && Str::Cmp(u8"你好", u8"你好嗎") < 0);

	using Wcs = Utils<wchar_t>;
	using WcsI = Utils<wchar_t, Advancing::as_ascii, Comparing::case_ignored>;
	using WcsN = Utils<wchar_t, Advancing::as_ascii, Comparing::regular, StringPolicy::Counter::cap_at_n>;
	using WcsNI = Utils<wchar_t, Advancing::as_ascii, Comparing::case_ignored, StringPolicy::Counter::cap_at_n>;

	static_assert(WcsI::Cmp(L"a0b1c2", L"A0B1C2") == 0);
	static_assert(WcsI::Cmp(L"abc", L"DEF") < 0 && Wcs::Cmp(L"abc", L"DEF") > 0);
	static_assert(WcsI::Cmp(L"GHI", L"def") > 0 && Wcs::Cmp(L"GHI", L"def") < 0);
	static_assert(Wcs::Cmp(L"你好", L"你好") == 0 && Wcs::Cmp(L"你好", L"你好嗎") < 0);
	static_assert(WcsN::Cmp(L"你好", L"你好嗎", 2) == 0 && WcsN::Cmp(L"你好", L"你好嗎", 3) < 0);
}

constexpr auto UTIL_Trim(std::string_view sv) noexcept -> decltype(sv)
{
	using namespace Hydrogenium::CCType;

	auto ret =
		sv
		| std::views::drop_while(IsSpace)
		| std::views::reverse
		| std::views::drop_while(IsSpace)
		| std::views::reverse;

	if (std::ranges::empty(ret))
		return "";

	return { std::addressof(*ret.begin()), std::ranges::size(ret) };
}

static_assert(UTIL_Trim("").empty());
static_assert(UTIL_Trim(" \r\n\t").empty());
static_assert(UTIL_Trim(" abc ") == "abc");
static_assert(UTIL_Trim(" abc") == "abc");
static_assert(UTIL_Trim("abc ") == "abc");
static_assert(UTIL_Trim("abc") == "abc");

// UTIL_ReplaceAll
// UTIL_Split