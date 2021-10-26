module;

// C++
#include <string>
#include <tuple>
#include <type_traits>

// C
#include <cassert>
#include <cctype>
#include <cwctype>

// Platform
#include <windows.h>

export module UtlString;

export template<typename chTy, size_t N>
struct StringLiteral
{
	// Reflection
	using Char_t = chTy;
	using This_t = StringLiteral<chTy, N>;
	using String_t = std::basic_string<chTy, std::char_traits<chTy>, std::allocator<chTy>>;	// This is how std::string and stuff like that defined in <xstring>
	using StringView_t = std::basic_string_view<chTy, std::char_traits<chTy>>;

	// Constructor
	constexpr StringLiteral(const chTy(&str)[N]) noexcept { std::copy_n(str, N, value); }

	// Operators
	constexpr operator const Char_t* () const noexcept { return &value[0]; }	// automatically convert to char* if necessary.
	constexpr operator Char_t* () noexcept { return &value[0]; }
	constexpr operator String_t() const noexcept { return String_t(&value[0], N); }
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

	// Constants
	static constexpr std::size_t length = N - 1U;	// Remove the '\0' at the end.

	// Members
	Char_t value[N];
};

export template<typename Ty>
void UTIL_Trim(Ty& str)
{
	using Char_t = std::decay_t<decltype(std::declval<Ty&>()[size_t{}])>;

	static const auto fnNotSpace = [](std::make_unsigned_t<Char_t> c)
	{
		if constexpr (std::is_same_v<Char_t, char> || std::is_same_v<Char_t, char8_t>)
			return !std::isspace(c);
		else if constexpr (std::is_same_v<Char_t, wchar_t>)
			return !std::iswspace(c);
	};

	str.erase(str.begin(), find_if(str.begin(), str.end(), fnNotSpace));	// L trim
	str.erase(find_if(str.rbegin(), str.rend(), fnNotSpace).base(), str.end());	// R trim. std::reverse_iterator<Iter>::base() represents the true position of reversed iterator.
}

export template<typename Ty>
void UTIL_ReplaceAll(Ty& str, const Ty& from, const Ty& to)
{
	if (from.empty())
		return;

	std::decay_t<decltype(Ty::npos)> start_pos = 0;
	while ((start_pos = str.find(from, start_pos)) != Ty::npos)
	{
		str.replace(start_pos, from.length(), to);
		start_pos += to.length();	// In case 'to' contains 'from', like replacing 'x' with 'yx'
	}
}

export template<typename Ty>
[[nodiscard]]
auto UTIL_RemoveBrackets(const Ty& str)
{
	using Char_t = std::decay_t<decltype(std::declval<Ty&>()[size_t{}])>;
	using String_t = std::basic_string<Char_t, std::char_traits<Char_t>, std::allocator<Char_t>>;	// This is how std::string and stuff like that defined in <xstring>

	String_t ret = str;
	ret.erase(0, 1);
	ret.erase(ret.length() - 1, 1);

	return ret;
}

export template<typename Ty>
auto UTIL_GetSpaceCount(const Ty& str)
{
	using Char_t = std::decay_t<decltype(std::declval<Ty&>()[size_t{}]) > ;

	static const auto fnNotSpace = [](std::make_unsigned_t<Char_t> c)
	{
		if constexpr (std::is_same_v<Char_t, char>)
			return !std::isspace(c);
		else if constexpr (std::is_same_v<Char_t, wchar_t>)
			return !std::iswspace(c);
	};

	return std::distance(str.begin(), find_if(str.begin(), str.end(), fnNotSpace));	// L Space
}

// a safe variant of sprintf which formatting strings.
export template <size_t size, typename ... Args>
decltype(auto) UTIL_sprintf(char(&dest)[size], const char* format, Args ... args)
{
	auto _return = _snprintf_s(dest, size - 1U, format, args ...);
	dest[size - 1U] = '\0';
	return _return;
}

// a safe variant of swprintf which formatting strings.
export template <size_t size, typename ... Args>
decltype(auto) UTIL_swprintf(wchar_t(&dest)[size], const wchar_t* format, Args ... args)
{
	auto _return = _snwprintf(dest, size - 1U, format, args ...);
	dest[size - 1U] = L'\0';
	return _return;
}

export template<typename Ty>
auto UTIL_RemoveXmlNode(const Ty& sz)
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

export template<typename From_t, typename To_t>
requires(sizeof(From_t) != sizeof(To_t))
[[nodiscard]]
auto UTIL_EncodingConversion(const From_t* pszFrom)
{
	using String_t = std::basic_string<To_t, std::char_traits<To_t>, std::allocator<To_t>>;
	using StringView_t = std::basic_string_view<To_t, std::char_traits<To_t>>;

	static std::make_signed_t<size_t> _iSize = sizeof(To_t) * 1U;
	static To_t* _pcBuffer = (To_t*)std::malloc(_iSize);	// #MEM_ALLOC_GLB
	static auto _fnConversion = [&](To_t* pBuf, size_t iSize)
	{
		if constexpr (std::is_same_v<From_t, wchar_t> && std::is_same_v<To_t, char>)	// Unicode to ASCII
			return ::WideCharToMultiByte(CP_ACP, 0, pszFrom, -1, pBuf, iSize, nullptr, nullptr);
		else if constexpr (std::is_same_v<From_t, char> && std::is_same_v<To_t, wchar_t>)	// ASCII to Unicode
			return ::MultiByteToWideChar(CP_ACP, 0, pszFrom, -1, pBuf, iSize);
		if constexpr (std::is_same_v<From_t, wchar_t> && std::is_same_v<To_t, char8_t>)	// Unicode to UTF-8
			return ::WideCharToMultiByte(CP_UTF8, 0, pszFrom, -1, reinterpret_cast<char*>(pBuf), iSize, nullptr, nullptr);
		else if constexpr (std::is_same_v<From_t, char8_t> && std::is_same_v<To_t, wchar_t>)	// UTF-8 to Unicode
			return ::MultiByteToWideChar(CP_UTF8, 0, reinterpret_cast<const char*>(pszFrom), -1, pBuf, iSize);

	};

	auto iTargetSize = _fnConversion(nullptr, 0);

	if (iTargetSize > _iSize)
	{
		_iSize = sizeof(To_t) * iTargetSize;
		_pcBuffer = (To_t*)std::realloc(_pcBuffer, _iSize);
	}

	if (iTargetSize == ERROR_NO_UNICODE_TRANSLATION)
		throw std::exception("Invalid utf8 sequence.");
	else if (!iTargetSize)
		throw std::exception("Error in conversion.");
	else if (_fnConversion(_pcBuffer, iTargetSize) != iTargetSize)
		throw std::exception("La falla!");

	return String_t(_pcBuffer);
}

export
[[nodiscard]]
struct DividedFile_t
{
	size_t	m_iSizeBefore{ 0U };
	size_t	m_iSizeAfter{ 0U };
	BYTE* m_pBinBefore{ nullptr };
	BYTE* m_pBinAfter{ nullptr };

	void Free(void) { if (m_pBinAfter) std::free(m_pBinAfter); if (m_pBinBefore) std::free(m_pBinBefore); m_pBinAfter = m_pBinBefore = nullptr; m_iSizeAfter = m_iSizeBefore = 0U; }
}
UTIL_GetBinaryStreamExceptKeyword(const char* szFilePath, const char* szKeyword)	// #RET_HEAP_MEM
{
	const long KEYWORD_LENGTH = static_cast<long>(std::strlen(szKeyword));
	BYTE* pszBuf = new BYTE[KEYWORD_LENGTH];	// #MEM_ALLOC

	FILE* pf = nullptr;
	fopen_s(&pf, szFilePath, "rb");
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
		.m_pBinBefore = (BYTE*)std::malloc(ret.m_iSizeBefore),
		.m_pBinAfter = (BYTE*)std::malloc(ret.m_iSizeAfter)
	};

	std::fseek(pf, 0, SEEK_SET);
	fread_s(ret.m_pBinBefore, ret.m_iSizeBefore, ret.m_iSizeBefore, 1, pf);
	std::fseek(pf, iKeywordPos + KEYWORD_LENGTH, SEEK_SET);
	fread_s(ret.m_pBinAfter, ret.m_iSizeAfter, ret.m_iSizeAfter, 1, pf);

	delete[] pszBuf;	// #MEM_FREED
	fclose(pf);
	return ret;
}

export int UTIL_GetStringType(const char* src)	// [0 - string] [1 - integer] [2 - float]
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

template<typename T, typename U>
concept ProperContainer = requires(T t, U u) { {t.emplace_back(u)}; };

export template<typename Container_t, typename String_t>
requires ProperContainer<Container_t, String_t>
void UTIL_Split(const String_t& s, Container_t& tokens, const String_t& delimiters)
{
	typename String_t::size_type lastPos = s.find_first_not_of(delimiters, 0);
	typename String_t::size_type pos = s.find_first_of(delimiters, lastPos);

	while (String_t::npos != pos || String_t::npos != lastPos)
	{
		tokens.emplace_back(s.substr(lastPos, pos - lastPos));
		lastPos = s.find_first_not_of(delimiters, pos);
		pos = s.find_first_of(delimiters, lastPos);
	}
}

export template<std::integral auto _iValue>
consteval auto UTIL_CountDigits(void)
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
consteval auto UTIL_GetDigit(void) requires(iWhichDigit < UTIL_CountDigits<iValue>())
{
	decltype(iValue) iDominator = 1;

	for (unsigned i = 0; i < iWhichDigit; i++)
		iDominator *= 10;

	return (iValue / iDominator) % 10;
}

template<unsigned iWhichDigit, std::integral auto iValue>
consteval auto UTIL_GetDigit_R(void) requires(iWhichDigit < UTIL_CountDigits<iValue>())
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
	constexpr operator const char* () const noexcept { return &value[0]; }	// automatically convert to char* if necessary.
	constexpr operator char* () noexcept { return &value[0]; }
	constexpr operator std::string() const noexcept { return std::string(&value[0], COUNT); }

	// Operator
	constexpr decltype(auto) operator[] (std::size_t index) const noexcept { assert(index < COUNT); return value[index]; }

	// Constants
	static constexpr size_t COUNT = UTIL_CountDigits<iValue>() + 1U;	// Additional '\0'
	static constexpr size_t LENGTH = UTIL_CountDigits<iValue>();

	// Member
	char value[COUNT];
};

export template<typename Char_t>
Char_t* UTIL_VarArgs(const Char_t* format, ...) noexcept
{
	static_assert(std::is_same_v<Char_t, char> || std::is_same_v<Char_t, wchar_t>, "This function supports only 'char' and 'wchar_t'.");

	va_list argptr;
	static constexpr size_t BUF_LEN = 2048;
	static Char_t rgsz[BUF_LEN];

	va_start(argptr, format);

	if constexpr (std::is_same_v<Char_t, char>)
		_vsnprintf_s(rgsz, BUF_LEN, format, argptr);
	else if constexpr (std::is_same_v<Char_t, wchar_t>)
		_vsnwprintf_s(rgsz, BUF_LEN, format, argptr);

	va_end(argptr);

	return rgsz;
}

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
