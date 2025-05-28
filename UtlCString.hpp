#pragma once

#include "UtlCChType.hpp"

#include <iostream>	// only use for unit test StrTok
#include <optional>
#include <ranges>
#include <string_view>
#include <string>

namespace Hydrogenium::CString
{
	using std::optional;
	using std::size_t;
	using std::string;
	using std::string_view;

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
		static thread_local const char* buffer = nullptr, * last_excl = nullptr;

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
