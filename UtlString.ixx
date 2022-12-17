module;

#pragma warning( push )
#pragma warning( disable : 4005 )
#pragma warning( disable : 5105 )
#pragma warning( disable : 5106 )

// C
#include <assert.h>
#include <stdio.h>

export module UtlString;

export import <cstring>;
export import <cwctype>;

// C++
export import <array>;
export import <charconv>;
export import <experimental/generator>;
export import <ranges>;
export import <string>;

// Friendly modules.
export import UtlConcepts;

export template <CharacterType chTy, size_t N>
struct StringLiteral
{
	// Reflection
	using Char_t = chTy;
	using This_t = StringLiteral<chTy, N>;
	using String_t = std::basic_string<chTy, std::char_traits<chTy>, std::allocator<chTy>>;	// This is how std::string and stuff like that defined in <xstring>
	using StringView_t = std::basic_string_view<chTy, std::char_traits<chTy>>;

	// Constructor
	constexpr StringLiteral(const chTy(&str)[N]) noexcept { std::copy_n(str, N, value); }
	explicit constexpr StringLiteral(const chTy* psz) noexcept { std::copy_n(psz, N, value); }

	// Operators
	constexpr operator const Char_t* () const noexcept { return &value[0]; }	// automatically convert to char* if necessary.
	constexpr operator String_t() const noexcept { return String_t(&value[0], length); }	// use length, or the '\0' thing will fall into the std::string itself. (terminator in the middle)
	constexpr decltype(auto) operator[] (std::size_t index) const noexcept { assert(index < N); return value[index]; }
	constexpr bool operator== (const String_t& rhs) const noexcept { return String_t(value, N) == rhs; }
	template<typename chTyOther, size_t sizeOther> constexpr bool operator== (const StringLiteral<chTyOther, sizeOther>& rhs) const noexcept
	{
		if constexpr (!std::is_same_v<Char_t, chTyOther> || N != sizeOther)
			return false;
		else
		{
			static constexpr auto IDX_SEQ = std::make_index_sequence<N>{};	// Theoretically I can skip the last character '\0'. But who knows... Perhaps one day we would meet a non-null terminal string.

			return [&] <size_t... I>(std::index_sequence<I...>) -> bool { return ((value[I] == rhs.value[I]) && ...); }(IDX_SEQ);	// Static strcmp.
		}
	}
	template<size_t sizeOther> constexpr decltype(auto) operator+ (const StringLiteral<Char_t, sizeOther>& rhs) const noexcept
	{
		constexpr auto IDX_SEQ_LHS = std::make_index_sequence<N>{};
		constexpr auto IDX_SEQ_RHS = std::make_index_sequence<sizeOther>{};
		constexpr auto NEW_COUNT = sizeOther + N - 1;	// Remove the dup '\0'

		Char_t rgsz[NEW_COUNT] = "\0";

		// Static strcpy.
		[&] <size_t... I>(std::index_sequence<I...>) { ((rgsz[I] = value[I]), ...); }(IDX_SEQ_LHS);
		[&] <size_t... I>(std::index_sequence<I...>) { ((rgsz[length + I] = rhs.value[I]), ...); }(IDX_SEQ_RHS);

		rgsz[NEW_COUNT - 1] = '\0';

		return StringLiteral<Char_t, NEW_COUNT>(rgsz);
	}

	// Methods
	constexpr This_t Replace(Char_t from, Char_t to) const noexcept
	{
		Char_t ret[N];
		std::copy_n(value, N, ret);

		for (auto& c : ret)
			if (c == from)
				c = to;

		return ret;
	}

	// Constants
	static constexpr std::size_t length = N - 1U;	// Remove the '\0' at the end.
	static constexpr std::size_t size = N;

	// Members
	Char_t value[N];

	// Iterators
	using value_type = Char_t;
	using iterator = _STD _Array_iterator<value_type, size>;
	using const_iterator = _STD _Array_const_iterator<value_type, size>;
	using reverse_iterator = _STD reverse_iterator<iterator>;
	using const_reverse_iterator = _STD reverse_iterator<const_iterator>;
	[[nodiscard]] constexpr iterator begin(void) noexcept { return iterator(value, 0); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator begin(void) const noexcept { return const_iterator(value, 0); }
	[[nodiscard]] constexpr iterator end(void) noexcept { return iterator(value, size); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator end(void) const noexcept { return const_iterator(value, size); }
	[[nodiscard]] constexpr reverse_iterator rbegin(void) noexcept { return reverse_iterator(end()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rbegin(void) const noexcept { return const_reverse_iterator(end()); }
	[[nodiscard]] constexpr reverse_iterator rend(void) noexcept { return reverse_iterator(begin()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rend(void) const noexcept { return const_reverse_iterator(begin()); }
	[[nodiscard]] constexpr const_iterator cbegin(void) const noexcept { return begin(); }
	[[nodiscard]] constexpr const_iterator cend(void) const noexcept { return end(); }
	[[nodiscard]] constexpr const_reverse_iterator crbegin(void) const noexcept { return rbegin(); }
	[[nodiscard]] constexpr const_reverse_iterator crend(void) const noexcept { return rend(); }
};

export template<StringLiteral A>	// C++ 20 feature.
consteval auto operator"" _s(void) noexcept
{
	return A;
}

export template <typename Ty>
void UTIL_Trim(Ty* str) noexcept
{
	using Char_t = std::decay_t<decltype(std::declval<Ty&>()[size_t{}])>;

	static constexpr auto fnNotSpace = [](std::make_unsigned_t<Char_t> c) noexcept
	{
		if constexpr (std::is_same_v<Char_t, char>/* || std::is_same_v<Char_t, char8_t>*/)
			return !std::isspace(c);
		else if constexpr (std::is_same_v<Char_t, wchar_t>)
			return !std::iswspace(c);
	};

	str->erase(str->begin(), find_if(str->begin(), str->end(), fnNotSpace));	// L trim
	str->erase(find_if(str->rbegin(), str->rend(), fnNotSpace).base(), str->end());	// R trim. std::reverse_iterator<Iter>::base() represents the true position of reversed iterator.
}

export
std::string_view UTIL_Trim(const std::string_view& str) noexcept
{
	auto const iFirstNotSpace = str.find_first_not_of(" \f\n\r\t\v");
	auto const iLastNotSpace = str.find_last_not_of(" \f\n\r\t\v");

	assert(iLastNotSpace >= iFirstNotSpace);

	if (iFirstNotSpace == str.npos && iLastNotSpace == str.npos)
		return "";
	else if (iFirstNotSpace == str.npos)
		return str.substr(0, iLastNotSpace);
	else if (iLastNotSpace == str.npos)
		return str.substr(iFirstNotSpace);
	else
		return str.substr(iFirstNotSpace, iLastNotSpace - iFirstNotSpace + 1);
}

export template<CharacterType Ty>
void UTIL_ReplaceAll(std::basic_string<Ty>& str, const std::basic_string_view<Ty>& from, const std::basic_string_view<Ty>& to) noexcept
{
	if (from.empty())
		return;

	std::size_t start_pos = 0;
	while ((start_pos = str.find(from, start_pos)) != str.npos)
	{
		str.replace(start_pos, from.length(), to);
		start_pos += to.length();	// In case 'to' contains 'from', like replacing 'x' with 'yx'
	}
}

export template<typename Ty>
[[nodiscard]]
auto UTIL_RemoveBrackets(const Ty& str) noexcept
{
	using Char_t = std::decay_t<decltype(std::declval<Ty&>()[size_t{}])>;
	using String_t = std::basic_string<Char_t, std::char_traits<Char_t>, std::allocator<Char_t>>;	// This is how std::string and stuff like that defined in <xstring>

	String_t ret = str;
	ret.erase(0, 1);
	ret.pop_back();

	return ret;
}

export template<typename Ty>
auto UTIL_GetSpaceCount(const Ty& str) noexcept
{
	using Char_t = std::decay_t<decltype(std::declval<Ty&>()[size_t{}]) > ;

	static const auto fnNotSpace = [](std::make_unsigned_t<Char_t> c)
	{
		if constexpr (std::is_same_v<Char_t, char>)
			return !std::isspace(c);
		else if constexpr (std::is_same_v<Char_t, wchar_t>)
			return !std::iswspace(c);
	};

	return std::distance(str.begin(), std::find_if(str.begin(), str.end(), fnNotSpace));	// L Space
}

export template<typename Ty>
auto UTIL_RemoveXmlNode(const Ty& sz) noexcept
{
	using Char_t = std::decay_t<decltype(std::declval<Ty&>()[size_t{}])>;
	using String_t = std::basic_string<Char_t, std::char_traits<Char_t>, std::allocator<Char_t>>;
	using StringView_t = std::basic_string_view<Char_t, std::char_traits<Char_t>>;

	auto iPos1 = sz.find_first_of('>') + 1;
	auto iPos2 = sz.find_last_of('<');

	if (iPos1 == String_t::npos || iPos2 == String_t::npos || iPos2 < iPos1)
		return sz;

	return sz.substr(iPos1, iPos2 - iPos1);
}

//export template<typename To_t, typename From_t>
//requires(sizeof(From_t) != sizeof(To_t))
//[[nodiscard]]
//auto UTIL_EncodingConversion(const From_t* pszFrom)
//{
//	using String_t = std::basic_string<To_t, std::char_traits<To_t>, std::allocator<To_t>>;
//	using StringView_t = std::basic_string_view<To_t, std::char_traits<To_t>>;
//
//	static std::make_signed_t<size_t> _iSize = sizeof(To_t) * 1U;
//	static To_t* _pcBuffer = (To_t*)std::malloc(_iSize);	// #MEM_ALLOC_GLB
//	static auto const _fnConversion = [&](To_t* pBuf, int iSize)
//	{
//		if constexpr (std::is_same_v<From_t, wchar_t> && std::is_same_v<To_t, char>)	// Unicode to ASCII
//			return ::WideCharToMultiByte(CP_ACP, 0, pszFrom, -1, pBuf, iSize, nullptr, nullptr);
//		else if constexpr (std::is_same_v<From_t, char> && std::is_same_v<To_t, wchar_t>)	// ASCII to Unicode
//			return ::MultiByteToWideChar(CP_ACP, 0, pszFrom, -1, pBuf, iSize);
//		if constexpr (std::is_same_v<From_t, wchar_t> && std::is_same_v<To_t, char8_t>)	// Unicode to UTF-8
//			return ::WideCharToMultiByte(CP_UTF8, 0, pszFrom, -1, reinterpret_cast<char*>(pBuf), iSize, nullptr, nullptr);
//		else if constexpr (std::is_same_v<From_t, char8_t> && std::is_same_v<To_t, wchar_t>)	// UTF-8 to Unicode
//			return ::MultiByteToWideChar(CP_UTF8, 0, reinterpret_cast<const char*>(pszFrom), -1, pBuf, iSize);
//	};
//
//	auto iTargetSize = _fnConversion(nullptr, 0);
//
//	if (iTargetSize > _iSize)
//	{
//		_iSize = sizeof(To_t) * iTargetSize;
//		_pcBuffer = (To_t*)std::realloc(_pcBuffer, _iSize);
//	}
//
//	if (iTargetSize == ERROR_NO_UNICODE_TRANSLATION)
//		throw std::format_error("Invalid utf8 sequence.");
//	else if (!iTargetSize)
//		throw std::runtime_error("Error in conversion.");
//	else if (_fnConversion(_pcBuffer, iTargetSize) != iTargetSize)
//		throw std::length_error("La falla!");
//
//	return String_t(_pcBuffer);
//}

export
[[nodiscard]]
struct DividedFile_t
{
	size_t	m_iSizeBefore{ 0U };
	size_t	m_iSizeAfter{ 0U };
	std::byte* m_pBinBefore{ nullptr };
	std::byte * m_pBinAfter{ nullptr };

	void Free(void) noexcept { if (m_pBinAfter) std::free(m_pBinAfter); if (m_pBinBefore) std::free(m_pBinBefore); m_pBinAfter = m_pBinBefore = nullptr; m_iSizeAfter = m_iSizeBefore = 0U; }
}
UTIL_GetBinaryStreamExceptKeyword(const char* szFilePath, const char* szKeyword) noexcept	// #RET_HEAP_MEM
{
	const long KEYWORD_LENGTH = static_cast<long>(std::strlen(szKeyword));
	std::byte *pszBuf = new std::byte[KEYWORD_LENGTH];	// #MEM_ALLOC

	FILE* pf = fopen(szFilePath, "rb");
	assert(pf != nullptr);

	std::fseek(pf, 0, SEEK_END);
	const auto FILE_SIZE = std::ftell(pf);

	std::fseek(pf, 0, SEEK_SET);
	std::invoke_result_t<decltype(std::ftell), FILE*> iKeywordPos = -1;
	std::invoke_result_t<decltype(std::fgetc), FILE*> c = EOF;
	while ((c = std::fgetc(pf)) != EOF)
	{
		auto iPosition = std::ftell(pf);	// Save & restore.

		fread_s(pszBuf, KEYWORD_LENGTH, KEYWORD_LENGTH, 1, pf);

		if (!std::memcmp(pszBuf, szKeyword, KEYWORD_LENGTH))
		{
			iKeywordPos = iPosition;
			break;
		}
		else
			std::fseek(pf, iPosition, SEEK_SET);	// Restore position.
	}

	if (iKeywordPos < 0)
		return DividedFile_t{};

	DividedFile_t ret
	{
		.m_iSizeBefore = static_cast<size_t>(iKeywordPos),
		.m_iSizeAfter = static_cast<size_t>(FILE_SIZE - iKeywordPos - KEYWORD_LENGTH),
		.m_pBinBefore = (std::byte *)std::malloc(ret.m_iSizeBefore),
		.m_pBinAfter = (std::byte *)std::malloc(ret.m_iSizeAfter)
	};

	std::fseek(pf, 0, SEEK_SET);
	fread_s(ret.m_pBinBefore, ret.m_iSizeBefore, ret.m_iSizeBefore, 1, pf);
	std::fseek(pf, iKeywordPos + KEYWORD_LENGTH, SEEK_SET);
	fread_s(ret.m_pBinAfter, ret.m_iSizeAfter, ret.m_iSizeAfter, 1, pf);

	delete[] pszBuf;	// #MEM_FREED
	fclose(pf);
	return ret;
}

export int UTIL_GetStringType(const char* src) noexcept	// [0 - string] [1 - integer] [2 - float]
{
	// is multi char ?
	if (*src <= 0)
		return 0;

	// is '-' or digit ?
	if (*src == '-' || isdigit(*src))
	{
		// "1"
		if (isdigit(*src) && !*(src + 1))
			return 1;

		++src; // next char

		// "-a" or "0a"
		if (!isdigit(*src) && *src != '.')
			return 0;

		while (*src)
		{
			// "1." or "-1."
			if (*src == '.')
			{
				++src; // next char

				// we need a digit, "1." not a float
				if (!*src)
					return 0;

				while (*src)
				{
					// "1.a"
					if (!isdigit(*src))
						return 0;

					++src;
				}
				// float value
				return 2;
			}

			// "10a" not a integer
			if (!isdigit(*src))
				return 0;

			++src; // next char
		}

		// integer value
		return 1;
	}

	return 0;
}

export
std::experimental::generator<std::string_view> UTIL_Split(std::string_view const& s, char const* delimiters) noexcept
{
	for (auto lastPos = s.find_first_not_of(delimiters, 0), pos = s.find_first_of(delimiters, lastPos);
		s.npos != pos || s.npos != lastPos;
		lastPos = s.find_first_not_of(delimiters, pos), pos = s.find_first_of(delimiters, lastPos)
		)
	{
		co_yield s.substr(lastPos, pos - lastPos);
	}

	co_return;
}

export template <Arithmetic T>
std::experimental::generator<T> UTIL_SplitIntoNums(std::string_view const& s, char const* delimiters) noexcept
{
	for (auto lastPos = s.find_first_not_of(delimiters, 0), pos = s.find_first_of(delimiters, lastPos);
		s.npos != pos || s.npos != lastPos;
		lastPos = s.find_first_not_of(delimiters, pos), pos = s.find_first_of(delimiters, lastPos)
		)
	{
		co_yield UTIL_StrToNum<T>(s.substr(lastPos, pos - lastPos));
	}

	co_return;
}

export template <Arithmetic T>
std::experimental::generator<std::pair<T, std::string_view>> UTIL_SplitIntoNumsWithStrRemainder(std::string_view const& s, char const* delimiters) noexcept
{
	for (auto lastPos = s.find_first_not_of(delimiters, 0), pos = s.find_first_of(delimiters, lastPos);
		s.npos != pos || s.npos != lastPos;
		lastPos = s.find_first_not_of(delimiters, pos), pos = s.find_first_of(delimiters, lastPos)
		)
	{
		co_yield std::make_pair(
			UTIL_StrToNum<T>(s.substr(lastPos, pos - lastPos)),
			s.substr(lastPos)
		);
	}

	co_return;
}

export template<std::integral auto _iValue>
consteval auto UTIL_CountDigits(void) noexcept
{
	unsigned iDigits = 0;
	auto iValue = _iValue;

	while (iValue)
	{
		iValue /= 10;
		iDigits++;
	}

	return iDigits;
}

export template<unsigned iWhichDigit, std::integral auto iValue>
consteval auto UTIL_GetDigit(void) noexcept requires(iWhichDigit < UTIL_CountDigits<iValue>())
{
	decltype(iValue) iDominator = 1;

	for (unsigned i = 0; i < iWhichDigit; i++)
		iDominator *= 10;

	return (iValue / iDominator) % 10;
}

template<unsigned iWhichDigit, std::integral auto iValue>
consteval auto UTIL_GetDigit_R(void) noexcept requires(iWhichDigit < UTIL_CountDigits<iValue>())
{
	decltype(iValue) iDominator = 1;
	constexpr unsigned iWhichDigit_R = UTIL_CountDigits<iValue>() - iWhichDigit - 1U;

	for (unsigned i = 0; i < iWhichDigit_R; i++)
		iDominator *= 10;

	return (iValue / iDominator) % 10;
}

export template<std::integral auto iValue>
requires(iValue >= 0)
struct UTIL_IntToString
{
	// Constructor
	consteval UTIL_IntToString() noexcept
	{
		[&] <size_t... I>(std::index_sequence<I...>)
		{
			CopyToCharArray(static_cast<char>('0' + UTIL_GetDigit_R<I, iValue>())...);
		}
		(std::make_index_sequence<UTIL_CountDigits<iValue>()>{});
	}

	// Method
	template<typename... Args_t>
	constexpr void CopyToCharArray(Args_t... args) noexcept requires(sizeof...(Args_t) > 0 && ((std::is_same_v<Args_t, char>) && ...))
	{
		auto tpl = std::make_tuple(args...);

		auto fnCopyToCharArray = [&]<size_t... I>(std::index_sequence<I...>)
		{
			((value[I] = std::get<I>(tpl)), ...);
		};

		fnCopyToCharArray(std::make_index_sequence<LENGTH>{});	// Not 'count' this time.
		value[LENGTH] = '\0';
	}

	// Conversion
	constexpr operator std::string_view () const noexcept { return std::string_view(&value[0]); }
	constexpr operator const char* () const noexcept { return &value[0]; }	// automatically convert to char* if necessary.

	// Operator
	constexpr decltype(auto) operator[] (std::size_t index) const noexcept { assert(index < COUNT); return value[index]; }

	// Constants
	static constexpr size_t COUNT = UTIL_CountDigits<iValue>() + 1U;	// Additional '\0'
	static constexpr size_t LENGTH = UTIL_CountDigits<iValue>();

	// Member
	char value[COUNT];
};

export
auto stristr(auto str, auto substr) noexcept requires(std::is_pointer_v<decltype(str)> && std::is_pointer_v<decltype(substr)>)
{
	decltype(str) p1 = str;
	decltype(substr) p2 = substr;
	auto r = *p2 == '\0' ? str : nullptr;

	while (*p1 != '\0' && *p2 != '\0')
	{
		if (tolower((unsigned char)*p1) == tolower((unsigned char)*p2))
		{
			if (!r)
			{
				r = p1;
			}

			p2++;
		}
		else
		{
			p2 = substr;
			if (r)
			{
				p1 = r + 1;
			}

			if (tolower((unsigned char)*p1) == tolower((unsigned char)*p2))
			{
				r = p1;
				p2++;
			}
			else
			{
				r = nullptr;
			}
		}

		p1++;
	}

	return *p2 == '\0' ? r : nullptr;
}

export
consteval size_t strlen_c(const CharacterType auto* str) noexcept
{
	return *str ? 1 + strlen_c(str + 1) : 0;
}

export template<Arithmetic T>
auto UTIL_StrToNum(const std::string_view& sz) noexcept
{
	if constexpr (std::is_enum_v<T>)
	{
		if (std::underlying_type_t<T> ret{}; std::from_chars(sz.data(), sz.data() + sz.size(), ret).ec == std::errc{})
			return static_cast<T>(ret);
	}
	else
	{
		if (T ret{}; std::from_chars(sz.data(), sz.data() + sz.size(), ret).ec == std::errc{})
			return ret;
	}

	return T{};
}

export template<size_t N>
requires(N > 1)
struct UTIL_SpacedFormatter
{
	consteval UTIL_SpacedFormatter() noexcept {}

	constexpr operator std::string_view() const noexcept { return value; }
	constexpr operator const char* () const noexcept { return _impl_rgc.data(); }

	template<size_t C>
	static consteval std::array<char, C * 3> _impl_fn(void) noexcept
	{
		std::array<char, C * 3> rgc;

		[&] <size_t... I>(std::index_sequence<I...> seq)
		{
			((rgc[0 + I * 3] = '{'), ...);
			((rgc[1 + I * 3] = '}'), ...);
			((rgc[2 + I * 3] = ' '), ...);

			rgc.back() = '\0';
		}
		(std::make_index_sequence<C>{});

		return rgc;
	}

	constexpr static auto _impl_rgc = _impl_fn<N>();
	constexpr static std::string_view value = std::string_view(_impl_rgc.data());
};

export template<size_t N>
requires(N > 1)
struct UTIL_SpaceCommaFormatter
{
	consteval UTIL_SpaceCommaFormatter() noexcept {}

	constexpr operator std::string_view() const noexcept { return value; }
	constexpr operator const char* () const noexcept { return _impl_rgc.data(); }

	template<size_t C>
	static consteval std::array<char, C * 4 - 1> _impl_fn(void) noexcept
	{
		std::array<char, C * 4 - 1> rgc;

		[&] <size_t... I>(std::index_sequence<I...> seq)
		{
			((rgc[0 + I * 4] = '{'), ...);
			((rgc[1 + I * 4] = '}'), ...);
		}
		(std::make_index_sequence<C>{});

		[&] <size_t... I>(std::index_sequence<I...> seq)
		{
			((rgc[2 + I * 4] = ','), ...);
			((rgc[3 + I * 4] = ' '), ...);
		}
		(std::make_index_sequence<C - 1>{});

		rgc.back() = '\0';
		return rgc;
	}

	constexpr static auto _impl_rgc = _impl_fn<N>();
	constexpr static std::string_view value = std::string_view(_impl_rgc.data());
};

export template<size_t N>
requires(N > 0)
struct UTIL_Indent
{
	consteval UTIL_Indent(void) noexcept {}

	constexpr operator std::string_view() const noexcept { return value; }
	constexpr operator const char* () const noexcept { return _impl_rgc.data(); }

	static consteval std::array<char, N + 1> _impl_fn(void) noexcept
	{
		std::array<char, N + 1> rgc;

		rgc.fill('\t');
		rgc.back() = '\0';

		return rgc;
	}

	constexpr static auto _impl_rgc = _impl_fn();
	constexpr static std::string_view value = std::string_view(_impl_rgc.data());
};

export std::string UTIL_IndentDyn(size_t iIndentCount) noexcept
{
	return std::string(iIndentCount, '\t');
}

export template<StringLiteral STR>
auto strnicmp_c(const char* pszOther) noexcept
{
	return _strnicmp(STR, pszOther, STR.length);
}

export
constexpr char toupper_c(char c) noexcept
{
	if (c >= 'a' && c <= 'z')
		return static_cast<char>(c - 'a' + 'A');

	return c;
}

export
constexpr char tolower_c(char c) noexcept
{
	if (c >= 'A' && c <= 'Z')
		return static_cast<char>(c - 'A' + 'a');

	return c;
}

export template<size_t iSizeLhs, size_t iSizeRhs>
constexpr auto stricmp_c(const char (&lhs)[iSizeLhs], const char (&rhs)[iSizeRhs])
{
	if constexpr (iSizeLhs != iSizeRhs)
		return iSizeLhs <=> iSizeRhs;

	constexpr auto iSize = iSizeLhs;	// Since they are the same...

	for (size_t i = 0; i < iSize; ++i)
	{
		if (auto ret = tolower_c(lhs[i]) <=> tolower_c(rhs[i]); ret != std::strong_ordering::equal)
			return ret;
	}

	return std::strong_ordering::equal;
}

//export template<HasIndexOperator T>
//T UTIL_Parse(const StlString auto& s, const auto& delimiters) noexcept
//{
//	using Cell_t = std::decay_t<decltype(std::declval<T&>()[size_t {}])>;
//	
//	static_assert(Arithmetic<Cell_t>, "Must be an arithmetic cell type.");
//
//	T ret;
//	auto lastPos = s.find_first_not_of(delimiters, 0);
//	auto pos = s.find_first_of(delimiters, lastPos);
//
//	// Support 4 cells at max.
//	for (size_t i = 0; i < 4 && (s.npos != pos || s.npos != lastPos); ++i)
//	{
//		ret[i] = UTIL_StrToNum<Cell_t>(s.substr(lastPos, pos - lastPos));
//		lastPos = s.find_first_not_of(delimiters, pos);
//		pos = s.find_first_of(delimiters, lastPos);
//	}
//
//	return ret;
//}

export template<CharacterType T>
size_t strrep(T* psz, T from, T to) noexcept
{
	size_t iCount = 0;

	for (; psz && *psz != '\0'; ++psz)
	{
		if (*psz == from)
		{
			*psz = to;
			++iCount;
		}
	}

	return iCount;
}

export
constexpr bool isdigit_c(char c) noexcept
{
	switch (c)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		return true;

	default:
		return false;
	}
}

export
constexpr bool isdigit_c(wchar_t c) noexcept
{
	switch (c)
	{
	case L'0':
	case L'1':
	case L'2':
	case L'3':
	case L'4':
	case L'5':
	case L'6':
	case L'7':
	case L'8':
	case L'9':
		return true;

	default:
		return false;
	}
}

export
constexpr bool isupper_c(char c) noexcept
{
	switch (c)
	{
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
		return true;

	default:
		return false;
	}
}

export
constexpr bool isupper_c(wchar_t c) noexcept
{
	switch (c)
	{
	case L'A':
	case L'B':
	case L'C':
	case L'D':
	case L'E':
	case L'F':
	case L'G':
	case L'H':
	case L'I':
	case L'J':
	case L'K':
	case L'L':
	case L'M':
	case L'N':
	case L'O':
	case L'P':
	case L'Q':
	case L'R':
	case L'S':
	case L'T':
	case L'U':
	case L'V':
	case L'W':
	case L'X':
	case L'Y':
	case L'Z':
		return true;

	default:
		return false;
	}
}

export
constexpr bool islower_c(char c) noexcept
{
	switch (c)
	{
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
		return true;

	default:
		return false;
	}
}

export
constexpr bool islower_c(wchar_t c) noexcept
{
	switch (c)
	{
	case L'a':
	case L'b':
	case L'c':
	case L'd':
	case L'e':
	case L'f':
	case L'g':
	case L'h':
	case L'i':
	case L'j':
	case L'k':
	case L'l':
	case L'm':
	case L'n':
	case L'o':
	case L'p':
	case L'q':
	case L'r':
	case L's':
	case L't':
	case L'u':
	case L'v':
	case L'w':
	case L'x':
	case L'y':
	case L'z':
		return true;

	default:
		return false;
	}
}

export
constexpr bool isspace_c(char c) noexcept
{
	switch (c)
	{
	case ' ':
	case '\f':
	case '\n':
	case '\r':
	case '\t':
	case '\v':
		return true;

	default:
		return false;
	}
}

export template <std::size_t iSize>
constexpr std::size_t strcnt(const unsigned char(&rgsz)[iSize]) noexcept
{
	std::size_t iStrLen = 0;

	for (auto &&c : rgsz)
	{
		if ((c < 0b10000000 && c > 0)
			|| (c & 0b11100000) == 0b11000000
			|| (c & 0b11110000) == 0b11100000
			|| (c & 0b11111000) == 0b11110000
			)
		{
			++iStrLen;
		}
	}

	return iStrLen;
}

/* Unit test for strcnt, UTF-8 character counter function.
* Current unavailable due to fucking MS.
* 
	static constexpr unsigned char pszu8Latin[] = u8"Heraclius";
	static constexpr unsigned char pszu8Greek[] = u8"Ἡράκλειος";
	static constexpr unsigned char pszu8French[] = u8"Héraclius";
	static constexpr unsigned char pszu8Russian[] = u8"Ираклий";
	static constexpr unsigned char pszu8Japanese[] = u8"ヘラクレイオス";
	static constexpr unsigned char pszu8Chinese[] = u8"希拉克略";

	//static_assert(strcnt(pszu8Latin) == 9);
	//static_assert(strcnt(pszu8Greek) == 9);
	//static_assert(strcnt(pszu8French) == 9);
	//static_assert(strcnt(pszu8Russian) == 7);
	//static_assert(strcnt(pszu8Japanese) == 7);
	//static_assert(strcnt(pszu8Chinese) == 4);

	std::cout << strcnt(pszu8Latin) << '\n';
	std::cout << strcnt(pszu8Greek) << '\n';
	std::cout << strcnt(pszu8French) << '\n';
	std::cout << strcnt(pszu8Russian) << '\n';
	std::cout << strcnt(pszu8Japanese) << '\n';
	std::cout << strcnt(pszu8Chinese) << '\n';

* Dynamic runtime function:
export
constexpr std::size_t strcnt(const unsigned char *psz) noexcept
{
	std::size_t iStrLen = 0;

	for (auto p = psz; *p != '\0'; ++p)
	{
		auto &&c = *p;

		if (c < 0b10000000
			|| (c & 0b11100000) == 0b11000000
			|| (c & 0b11110000) == 0b11100000
			|| (c & 0b11111000) == 0b11110000
			)
		{
			++iStrLen;
		}
	}

	return iStrLen;
}
*/

export
inline constexpr auto front_trim = std::views::drop_while(isspace_c);

export
inline constexpr auto back_trim = std::views::reverse | std::views::drop_while(isspace_c) | std::views::reverse;	// #FIXME doesn't work, don't know why.

#pragma warning( pop )
