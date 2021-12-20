module;

// C++
#include <fstream>
#include <string>

// C
#include <cassert>
#include <sys/stat.h>

// Platform
#include <windows.h>

export module UtlFileSystem;

// Using string module.
import UtlString;

export inline void UTIL_InitializeXml(const char* szFilePath)	// Initialize as UTF-8-BOM
{
	static constexpr unsigned char smarker[] = { 0xEF, 0xBB, 0xBF };
	static const std::string szBasicXMLInfo = reinterpret_cast<const char*>(u8"<?xml version=\"1.0\" encoding=\"utf-8\"?>");	// Fuck C++20.

	if (std::ofstream fs(szFilePath, std::ios::out | std::ios::binary); fs)
	{
		fs.write(reinterpret_cast<const char*>(smarker), sizeof(smarker));	// Write() is for binary byte writing. Must be using with reinterpret_cast<>
		fs << szBasicXMLInfo << "\r\n" << std::flush;	// If std::endl is used here, it would cause the file being saved as LF. By doing this, it would enforce the file become CR-LF.
		fs.close();
	}
}

export template<typename Ty>
size_t UTIL_CountActualLinesInFile(const Ty* szFilePath)
{
	FILE* pFile = nullptr;
	if constexpr (std::is_same_v<Ty, wchar_t>)
		_wfopen_s(&pFile, szFilePath, L"r");
	else if constexpr (std::is_same_v<Ty, char>)
		fopen_s(&pFile, szFilePath, "r");

	if (pFile)
	{
		size_t iLineCount = 0;
		std::invoke_result_t<decltype(std::getwc), FILE*> c = WEOF, last_c = WEOF;

		while ((c = std::getwc(pFile)) != WEOF)
		{
			if (c == L'\n' && last_c != L'\n')
			{
				iLineCount++;
			}

			last_c = c;
		}

		fclose(pFile);
		return iLineCount;
	}

	return 0;
}

export template<StringLiteral _szKeyword>
[[nodiscard]]
auto UTIL_GetBinaryStreamBeforeKeyword(const char* szFilePath, std::size_t* piResultSize = nullptr)	// #RET_HEAP_MEM
{
	using BYTE = unsigned char;
	using chTy = decltype(_szKeyword)::Char_t;
	using stringTy = std::basic_string<chTy, std::char_traits<chTy>, std::allocator<chTy>>;	// This is how std::string and stuff like that defined in <xstring>
	static constexpr auto IDX_SEQ = std::make_index_sequence<_szKeyword.length>{};	// Do not use 'count'. It would enforce comparison of the terminal character '\0' which never exists in normal text file.

	static_assert(std::is_same_v<chTy, char> || std::is_same_v<chTy, wchar_t>, "This function supports only <char> and <wchar_t>.");

	FILE* pf = nullptr;
	fopen_s(&pf, szFilePath, "rb");
	assert(pf != nullptr);

	std::fseek(pf, 0, SEEK_END);
	const auto iFileSize = std::ftell(pf);

	std::decay_t<decltype(iFileSize)> iSeekStartingPos = iFileSize;

LAB_SEARCH_ITERATION:;
	iSeekStartingPos -= _szKeyword.length;	// Search step.
	assert(iSeekStartingPos >= 0);			// You can't and shouldn't search from... previous file?
	fseek(pf, iSeekStartingPos, SEEK_SET);	// Reading from back of file.

//	auto fn = [](FILE* pf) { auto c = std::fgetc(pf); cout << static_cast<char>(c) << endl; return c; };

	long iEndPos = -1;
	std::invoke_result_t<decltype(std::fgetc), FILE*> c = EOF;
	while ((c = std::fgetc(pf)) != EOF)
	{
		auto iPosition = std::ftell(pf);	// Save & restore.

		bool bIsCurrentPosMatches = [&pf] <size_t... I>(std::index_sequence<I...>) -> bool
		{
//			cout << "new inspection starts.\n\n";
			return ((static_cast<chTy>(std::fgetc(pf)) == _szKeyword[I]) && ...);	// Static strcmp().
		}
		(IDX_SEQ);	// Instant call. Hence it will return a boolean rather than a lambda object.

		if (bIsCurrentPosMatches)
		{
			iEndPos = std::ftell(pf) - _szKeyword.length;
			break;
		}
		else
			std::fseek(pf, iPosition, SEEK_SET);	// Restore position.
	}

	if (iEndPos <= 0)
		goto LAB_SEARCH_ITERATION;

	BYTE* pTextAbove = (BYTE*)std::malloc(iEndPos);
	fseek(pf, 0, SEEK_SET);
	fread_s(pTextAbove, iEndPos, iEndPos, 1, pf);

	if (piResultSize)
		*piResultSize = iEndPos;

	fclose(pf);
	return pTextAbove;
}

std::wstring UTF8ToUnicode(const std::string& utf8string)
{
	static int _iSize = sizeof(wchar_t) * 1U;
	static wchar_t* _pwcBuffer = (wchar_t*)std::malloc(_iSize);	// #MEM_ALLOC_GLB

	auto wideSize = ::MultiByteToWideChar(CP_UTF8, 0, utf8string.c_str(), -1, nullptr, 0);

	if (wideSize > _iSize)
	{
		_iSize = sizeof(wchar_t) * wideSize;
		_pwcBuffer = (wchar_t*)std::realloc(_pwcBuffer, _iSize);
	}

	if (wideSize == ERROR_NO_UNICODE_TRANSLATION)
		throw std::exception("Invalid utf8 sequence.");
	else if (!wideSize)
		throw std::exception("Error in conversion.");
	else if (::MultiByteToWideChar(CP_UTF8, 0, utf8string.c_str(), -1, _pwcBuffer, wideSize) != wideSize)
		throw std::exception("La falla!");

	return std::wstring(_pwcBuffer);
}

std::string UnicodeToASCII(const std::wstring& unicodeString)
{
	static int _iSize = sizeof(char) * 1U;
	static char* _pcBuffer = (char*)std::malloc(_iSize);	// #MEM_ALLOC_GLB

	auto asciiSize = ::WideCharToMultiByte(CP_OEMCP, 0, unicodeString.c_str(), -1, nullptr, 0, nullptr, nullptr);

	if (asciiSize > _iSize)
	{
		_iSize = sizeof(char) * asciiSize;
		_pcBuffer = (char*)std::realloc(_pcBuffer, _iSize);
	}

	if (asciiSize == ERROR_NO_UNICODE_TRANSLATION)
		throw std::exception("Invalid utf8 sequence.");
	else if (!asciiSize)
		throw std::exception("Error in conversion.");
	else if (::WideCharToMultiByte(CP_OEMCP, 0, unicodeString.c_str(), -1, _pcBuffer, asciiSize, nullptr, nullptr) != asciiSize)
		throw std::exception("La falla!");

	return std::string(_pcBuffer);
}

export auto UTIL_GetFileSize(const char* psz) noexcept
{
	static struct stat stat_buf;
	return stat(psz, &stat_buf) == 0 ? stat_buf.st_size : -1;
}
