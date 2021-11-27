module;

#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS	// This is a very unsafe module.
#endif

// C++
#include <concepts>	// std::integral, etc...
#include <format>	// std::format
#include <string>	// stof

// C
#include <cctype>	// isspace isdigit
#include <cmath>	// roundf
#include <cstdio>	// fopen fclose fread fwrite
#include <cstdlib>	// atoi atof
#include <cstring>	// malloc free strlen

export module UtlKeyValues;

import UtlArithmetic;
import UtlBuffer;
import UtlColor;
import UtlConcepts;
import UtlLinearAlgebra;
import UtlString;

template<typename T> concept ConvertibleToArray2 = std::same_as<std::decay_t<T>, Vector2D>;
template<typename T> concept ConvertibleToArray3 = std::same_as<std::decay_t<T>, Vector>;
template<typename T> concept ConvertibleToArray4 = std::same_as<std::decay_t<T>, Quaternion> || std::same_as<std::decay_t<T>, Color4b> || std::same_as<std::decay_t<T>, Color4f>;
template<typename T> concept Savables = ConvertibleToArray2<T> || ConvertibleToArray3<T> || ConvertibleToArray4<T>;

// Entry - Subkey or KeyValue
// Subkey - Contains only Entries, but no value for itself.
// KeyValue - A pair of string.
// Key - the "Name" of a Subkey.
// Value - the "Number/String" part of a KeyValue.

export struct ValveKeyValues
{
	ValveKeyValues(const char* pszName) noexcept
	{
		Init();
		SetName(pszName);
	}
	virtual ~ValveKeyValues(void) noexcept
	{
		RemoveEverything();
	}

	// set/get name
	const char* GetName(void) const noexcept
	{
		if (m_pszName)
			return m_pszName;

		return "";
	}
	void SetName(const char* pszName) noexcept
	{
		if (!pszName)
			pszName = "";

		if (m_pszName)
		{
			free(m_pszName);
			m_pszName = nullptr;
		}

		size_t len = strlen(pszName);
		m_pszName = (char*)malloc(len + 1);
		strcpy(m_pszName, pszName);
	}

	// load/save file
	bool LoadFromFile(const char* resourceName) noexcept
	{
		FILE* f = fopen(resourceName, "rb");
		if (!f)
			return false;

		int fileSize;

		fseek(f, 0, SEEK_END);
		fileSize = ftell(f);
		fseek(f, 0, SEEK_SET);

		char* buffer = (char*)malloc(fileSize + 1);

		fread(buffer, fileSize, 1, f);

		fclose(f);

		buffer[fileSize] = 0;
		LoadFromBuffer(buffer);

		free(buffer);

		return true;
	}
	bool SaveToFile(const char* resourceName) noexcept
	{
		FILE* f = fopen(resourceName, "wb");
		if (!f)
			return false;

		CBuffer buf(0x10000); // 64KB
		RecursiveSaveToBuffer(buf, 0);

		fwrite(buf.Get(), buf.Tell(), 1, f);

		fclose(f);

		return true;
	}

	// load from text buffer
	bool LoadFromBuffer(const char* pBuffer) noexcept
	{
		CBuffer buf;
		char token[100];
		bool got;

		buf.Write(pBuffer, strlen(pBuffer) + 1);
		buf.Seek(seek::set, 0);

		RemoveEverything();

		ValveKeyValues* pPreviousKey = nullptr;
		ValveKeyValues* pCurrentKey = this;

		while (true)
		{
			// get the key
			got = ReadToken(token, buf);

			// expected 'ident' but end of file
			if (!got)
				break;

			if (!pCurrentKey)
			{
				pCurrentKey = new ValveKeyValues(token);

				if (pPreviousKey)
				{
					pPreviousKey->m_pPeer = pCurrentKey;
				}
			}
			else
			{
				// set name for this key
				pCurrentKey->SetName(token);
			}

			// get the value
			got = ReadToken(token, buf);

			// expected 'ident' or '{' but end of file
			if (!got)
				break;

			// get the key
			if (token[0] == '{')
			{
				pCurrentKey->RecursiveLoadFromBuffer(buf);
			}

			pPreviousKey = pCurrentKey;
			pCurrentKey = nullptr;
		}

		return true;
	}
	bool SaveToBuffer(char* pBuffer, size_t* piSize) noexcept
	{
		CBuffer buf(0x10000);
		RecursiveSaveToBuffer(buf, 0);

		memcpy(pBuffer, buf.Get(), buf.Tell());

		*piSize = buf.Tell();

		return true;
	}

	// find an entry
	ValveKeyValues* FindEntry(const char* pszName, bool bCreate = false) noexcept
	{
		if (!pszName || !pszName[0])
			return this;

		ValveKeyValues* lastItem = nullptr;
		ValveKeyValues* dat;

		for (dat = m_pSub; dat != nullptr; dat = dat->m_pPeer)
		{
			lastItem = dat;

			if (!strcmp(pszName, dat->m_pszName))
			{
				break;
			}
		}

		if (!dat)
		{
			if (bCreate)
			{
				dat = new ValveKeyValues(pszName);

				if (lastItem)
				{
					lastItem->m_pPeer = dat;
				}
				else
				{
					m_pSub = dat;
				}
			}
			else
			{
				return nullptr;
			}
		}

		return dat;
	}

	// craete an entry
	ValveKeyValues* CreateEntry(const char* pszName = nullptr) noexcept	// nullptr on pszName to represent a auto-index entry name.
	{
		ValveKeyValues* dat = nullptr;

		if (!pszName)
		{
			int index = 1;

			for (ValveKeyValues* dat = m_pSub; dat != nullptr; dat = dat->m_pPeer)
			{
				if (auto val = atoi(dat->GetName()); index <= val)
					index = val + 1;
			}

			dat = new ValveKeyValues(std::to_string(index).c_str());
		}
		else
			dat = new ValveKeyValues(pszName);

		AddEntry(dat);
		return dat;
	}

	// Deletes an entry
	bool DeleteEntry(const char* pszName) noexcept
	{
		if (!pszName || !pszName[0])
			return false;

		ValveKeyValues* dat = nullptr;
		for (dat = m_pSub; dat != nullptr; dat = dat->m_pPeer)
		{
			if (!strcmp(pszName, dat->m_pszName))
				break;
		}

		if (!dat)
			return false;

		delete dat;
		return true;
	}

	// add/remove an existing entry
	void AddEntry(ValveKeyValues* pEntry) noexcept
	{
		if (m_pSub == nullptr)
		{
			m_pSub = pEntry;
		}
		else
		{
			ValveKeyValues* pTempDat = m_pSub;

			while (pTempDat->GetNextEntry() != nullptr)
			{
				pTempDat = pTempDat->GetNextEntry();
			}

			pTempDat->m_pPeer = pEntry;
		}
	}
	void RemoveEntry(ValveKeyValues* pEntry) noexcept
	{
		if (!pEntry)
			return;

		if (m_pSub == pEntry)
		{
			m_pSub = m_pSub->m_pPeer;
		}
		else
		{
			ValveKeyValues* dat = m_pSub;

			while (dat->m_pPeer)
			{
				if (dat->m_pPeer == pEntry)
				{
					dat->m_pPeer = dat->m_pPeer->m_pPeer;
					break;
				}

				dat = dat->m_pPeer;
			}
		}

		pEntry->m_pPeer = nullptr;
	}

	// get entry
	ValveKeyValues* GetFirstEntry(void) const noexcept
	{
		return m_pSub;
	}
	ValveKeyValues* GetNextEntry(void) const noexcept
	{
		return m_pPeer;
	}
	ValveKeyValues* GetFirstSubkey(void) const noexcept
	{
		ValveKeyValues* dat = m_pSub;

		while (dat && !dat->m_pSub)
		{
			dat = dat->m_pPeer;
		}

		return dat;
	}
	ValveKeyValues* GetNextSubkey(void) const noexcept
	{
		ValveKeyValues* dat = m_pPeer;

		while (dat && !dat->m_pSub)
		{
			dat = dat->m_pPeer;
		}

		return dat;
	}
	ValveKeyValues* GetFirstKeyValue(void) const noexcept
	{
		ValveKeyValues* dat = m_pSub;

		while (dat && dat->m_pSub)
		{
			dat = dat->m_pPeer;
		}

		return dat;
	}
	ValveKeyValues* GetNextKeyValue(void) const noexcept
	{
		ValveKeyValues* dat = m_pPeer;

		while (dat && dat->m_pSub)
		{
			dat = dat->m_pPeer;
		}

		return dat;
	}

	// return true if not subkey or value included.
	bool IsEmpty(void) const noexcept
	{
		return !m_pSub && !m_pszValue;
	}
	bool IsKeyValue(void) const noexcept
	{
		return m_pszValue != nullptr;
	}
	bool IsSubkey(void) const noexcept
	{
		return m_pSub != nullptr;
	}

	// value, nullptr if inquerying self.
	template<typename T> T GetValue(const char* pszSubkeyName = nullptr) noexcept
	{
		ValveKeyValues* dat = FindEntry(pszSubkeyName);
		if (!dat || !dat->m_pszValue)
		{
			if constexpr (std::convertible_to<T, std::string>)
				return "";
			else
				return T{};
		}

		if constexpr (std::floating_point<T>)
		{
			return static_cast<T>(dat->m_flValue);
		}
		else if constexpr (std::integral<T>)
		{
			return static_cast<T>(std::round(dat->m_flValue));
		}
		else if constexpr (std::convertible_to<T, std::string_view>)
		{
			return dat->m_pszValue;
		}
		else
		{
			if (!dat->m_pszValue)
				return T();

			char* p0 = _strdup(dat->m_pszValue);	// #MEM_ALLOC
			const char* pEnd = p0 + strlen(p0) + 1;
			short iCount = 0;
			T ret;

			const auto fnCheckBoundary = [&](void) -> bool
			{
				if constexpr (ResizableContainer<T>)
					return true;
				else if constexpr (Iterable<T>)
					return iCount < std::distance(std::begin(ret), std::end(ret));
				else if constexpr (ConvertibleToArray4<T>)
					return iCount < 4;
				else if constexpr (ConvertibleToArray3<T>)
					return iCount < 3;
				else if constexpr (ConvertibleToArray2<T>)
					return iCount < 2;
				else
					static_assert(std::false_type::value, "Unsupported type involved.");
			};

			for (char* p = p0, *p1 = p0; p && p != pEnd && fnCheckBoundary(); /* Do nothing */)
			{
				switch (*p)
				{
				case ' ':
				case '\f':
				case '\n':
				case '\r':
				case '\t':
				case '\v':
					*p = '\0';
					[[fallthrough]];

				case '\0':	// The last one.
					if constexpr (ResizableContainer<T>)
						ret.emplace_back(UTIL_StrToNum<std::decay_t<decltype(std::declval<T&>()[size_t{}])>>(p1));
					else
						ret[iCount++] = UTIL_StrToNum<std::decay_t<decltype(std::declval<T&>()[size_t{}])>>(p1);

					p1 = ++p;
					break;

				default:
					++p;
					break;
				}
			}

			free(p0); p0 = nullptr;	// #MEM_FREED
			return ret;
		}
	}
	template<typename... Tys> std::tuple<Tys...> GetValue(const char* pszSubkeyName = nullptr) noexcept requires(sizeof...(Tys) > 1)
	{
		ValveKeyValues* dat = FindEntry(pszSubkeyName);
		if (!dat || !dat->m_pszValue)
			return {};

		char* p0 = _strdup(dat->m_pszValue);	// #MEM_ALLOC
		bool bStrtokInited = false;
		const auto fnStrtokWrapper = [&](void) -> const char*
		{
			char* pRet = nullptr;

			if (!bStrtokInited)
			{
				bStrtokInited = true;
				pRet = std::strtok(p0, " \f\n\r\t\v");
			}
			else
				pRet = std::strtok(nullptr, " \f\n\r\t\v");

			return pRet ? pRet : "";
		};

		// Fuck C++.
		// So much trouble was caused due to that order of evaluation of arguments is unspecified.
		// https://stackoverflow.com/questions/2934904/order-of-evaluation-in-c-function-parameters

		const auto fn = [&]<typename T>(void) -> T
		{
			if constexpr (Arithmetic<T>)
				return UTIL_StrToNum<T>(fnStrtokWrapper());
			else if constexpr (std::convertible_to<T, std::string_view>)
				return fnStrtokWrapper();
			else if constexpr (Savables<T>)
			{
				// Something went off here with MSVC or C++ standard.
				// When I move this line into lambda fn2(), MemTy will guaranteed to be type 'double'
				// WHY??? #POTENTIAL_BUG
				using MemTy = std::decay_t<decltype(std::declval<T>()[0])>;

				const auto fn2 = [&]<size_t... I>(std::index_sequence<I...> seq) -> T
				{
					std::array<MemTy, seq.size()> rg{};
					((rg[I] = UTIL_StrToNum<MemTy>(fnStrtokWrapper())), ...);
					return T(rg[I]...);
				};

				if constexpr (ConvertibleToArray2<T>)
					return fn2(std::make_index_sequence<2>{});
				else if constexpr (ConvertibleToArray3<T>)
					return fn2(std::make_index_sequence<3>{});
				else if constexpr (ConvertibleToArray4<T>)
					return fn2(std::make_index_sequence<4>{});
			}
			else
				static_assert(std::false_type::value, "Unsupported type involved!");
		};

		using Return_t = std::tuple<Tys...>;

		Return_t tpl;

		[&] <size_t... I>(std::index_sequence<I...>)
		{
			((std::get<I>(tpl) = fn.template operator() <std::tuple_element_t<I, Return_t>> ()), ...);
		}
		(std::make_index_sequence<std::tuple_size_v<Return_t>>{});

		free(p0);	// #MEM_FREED
		return tpl;
	}
	template<typename T> bool SetValue(const char* pszSubkeyName, const T& Value) noexcept	// Create new entry on no found.
	{
		ValveKeyValues* dat = FindEntry(pszSubkeyName, true);
		if (!dat)
			return false;

		if (dat->m_pszValue)
		{
			free(dat->m_pszValue);
			dat->m_pszValue = nullptr;
		}

		if constexpr (std::integral<T> || std::floating_point<T>)
		{
			const auto iCount = std::formatted_size("{}", Value) + 1;
			dat->m_pszValue = (char*)calloc(iCount, sizeof(char));
			std::format_to_n(dat->m_pszValue, iCount - 1, "{}", Value);

			dat->m_flValue = static_cast<double>(Value);
		}
		else if constexpr (Savables<T>)
		{
			const auto fn = [&]<size_t... I>(std::index_sequence<I...> seq)
			{
				const auto iCount = std::formatted_size(UTIL_SpacedFormatter<seq.size()>(), Value[I]...) + 1;
				dat->m_pszValue = (char*)calloc(iCount, sizeof(char));
				std::format_to_n(dat->m_pszValue, iCount - 1, UTIL_SpacedFormatter<seq.size()>(), Value[I]...);
			};

			if constexpr (ConvertibleToArray2<T>)
				fn(std::make_index_sequence<2>{});
			else if constexpr (ConvertibleToArray3<T>)
				fn(std::make_index_sequence<3>{});
			else
			{
				if (IsColor<T> && Value[3] == 0)
					fn(std::make_index_sequence<3>{});
				else
					fn(std::make_index_sequence<4>{});
			}

			dat->m_flValue = 0;
		}
		else if constexpr (std::convertible_to<T, std::string_view>)
		{
			if constexpr (std::is_same_v<std::decay_t<T>, char*>)
			{
				size_t len = strlen(Value);
				dat->m_pszValue = (char*)malloc(len + 1);
				strcpy(dat->m_pszValue, Value);
			}
			else	// i.e. std::string.
			{
				dat->m_pszValue = (char*)malloc(Value.length() + 1);
				strcpy_s(dat->m_pszValue, Value.length() + 1, Value.data());
			}

			switch (UTIL_GetStringType(dat->m_pszValue))
			{
			default:
			case 0:	// String
				dat->m_flValue = 0;
				break;

			case 1:	// Integer
			case 2:	// Floating point
				dat->m_flValue = std::atof(dat->m_pszValue);
				break;
			}
		}
		else if constexpr (Array<T>)	// Take fixed arrays first.
		{
			constexpr auto iArraySize = std::size(T{});	// Had to create one instance here. std::decltype<> doesn't work. But this is compile-time thing, it's alright I guess..?

			[&] <size_t... I>(std::index_sequence<I...>)
			{
				const auto iCount = std::formatted_size(UTIL_SpacedFormatter<iArraySize>(), Value[I]...) + 1;
				dat->m_pszValue = (char*)calloc(iCount, sizeof(char));
				std::format_to_n(dat->m_pszValue, iCount - 1, UTIL_SpacedFormatter<iArraySize>(), Value[I]...);
			}
			(std::make_index_sequence<iArraySize>{});
		}
		else if constexpr (Iterable<T>)	// For dynamic containers.
		{
			std::string szBuf;
			for (auto it = std::begin(Value); it != std::end(Value); ++it)
				szBuf += std::format("{} ", *it);

			szBuf.pop_back();	// Remove the last ' '.

			dat->m_pszValue = (char*)calloc(szBuf.length() + 1, sizeof(char));
			strcpy_s(dat->m_pszValue, szBuf.length() + 1, szBuf.c_str());
		}
		else
		{
			static_assert(std::false_type::value, "Unsupported type involved.");
		}

		return true;
	}
	template<typename... Tys> bool SetValue(const char* pszSubkeyName, const Tys&... Values) noexcept requires(sizeof...(Values) > 1)	// Create new entry on no found.
	{
		ValveKeyValues* dat = FindEntry(pszSubkeyName, true);
		if (!dat)
			return false;

		if (dat->m_pszValue)
		{
			free(dat->m_pszValue);
			dat->m_pszValue = nullptr;
		}

		constexpr std::string_view pszFormatter = UTIL_SpacedFormatter<sizeof...(Values)>();
		const auto iCount = std::formatted_size(pszFormatter, Values...) + 1;

		dat->m_pszValue = (char*)calloc(iCount, sizeof(char));
		std::format_to_n(dat->m_pszValue, iCount - 1, pszFormatter, Values...);

		dat->m_flValue = 0;

		return true;
	}

	// remove all key/value
	virtual void Clear(void) noexcept
	{
		delete m_pSub;
		m_pSub = nullptr;
	}
	virtual void deleteThis(void) noexcept
	{
		delete this;
	}

private:
	void RemoveEverything(void) noexcept
	{
		ValveKeyValues* dat;
		ValveKeyValues* datNext = nullptr;

		for (dat = m_pSub; dat != nullptr; dat = datNext)
		{
			datNext = dat->m_pPeer;
			dat->m_pPeer = nullptr;
			delete dat;
		}

		for (dat = m_pPeer; dat && dat != this; dat = datNext)
		{
			datNext = dat->m_pPeer;
			dat->m_pPeer = nullptr;
			delete dat;
		}

		m_flValue = 0;

		if (m_pszValue)
		{
			free(m_pszValue);
			m_pszValue = nullptr;
		}
	}

	void RecursiveLoadFromBuffer(CBuffer& buf) noexcept
	{
		char token[2048];
		bool got;
		int type;

		while (1)
		{
			// get the key
			got = ReadToken(token, buf);

			// expected 'ident' or '}' but end of file
			if (!got)
				break;

			// close the key
			if (token[0] == '}')
				break;

			ValveKeyValues* dat = CreateEntry(token);

			// get the value
			got = ReadToken(token, buf);

			// expected '{' or 'ident' but end of file
			if (!got)
				break;

			// expected '{' or 'ident' but got '}'
			if (token[0] == '}')
				break;

			if (token[0] == '{')
			{
				dat->RecursiveLoadFromBuffer(buf);
			}
			else
			{
				type = UTIL_GetStringType(token);

				if (type == 1 || type == 2)
					dat->m_flValue = std::stof(token);

				if (dat->m_pszValue)
				{
					free(dat->m_pszValue);
					dat->m_pszValue = nullptr;
				}

				size_t len = strlen(token);
				dat->m_pszValue = (char*)malloc(len + 1);
				strcpy_s(dat->m_pszValue, len + 1, token);
			}
		}
	}
	void RecursiveSaveToBuffer(CBuffer& buf, int indentLevel) noexcept
	{
		WriteIndents(buf, indentLevel);
		buf.Write("\"", 1);
		buf.Write(m_pszName, strlen(m_pszName));
		buf.Write("\"\n", 2);
		WriteIndents(buf, indentLevel);
		buf.Write("{\n", 2);

		for (ValveKeyValues* dat = m_pSub; dat != nullptr; dat = dat->m_pPeer)
		{
			if (dat->m_pSub)
			{
				dat->RecursiveSaveToBuffer(buf, indentLevel + 1);
			}
			else
			{
				WriteIndents(buf, indentLevel + 1);
				buf.Write("\"", 1);
				buf.Write(dat->GetName(), strlen(dat->GetName()));
				buf.Write("\"\t\t\"", 4);
				buf.Write(dat->GetValue<const char*>(), strlen(dat->GetValue<const char*>()));
				buf.Write("\"\n", 2);
			}
		}

		WriteIndents(buf, indentLevel);
		buf.Write("}\n", 2);
	}

	void WriteIndents(CBuffer& buf, int indentLevel) noexcept
	{
		for (int i = 0; i < indentLevel; ++i)
		{
			buf.Write("\t", 1);
		}
	}

	void Init(void) noexcept
	{
		if (m_pszName)
		{
			free(m_pszName);
			m_pszName = nullptr;
		}

		if (m_pszValue)
		{
			free(m_pszValue);
			m_pszValue = nullptr;
		}

		m_flValue = 0;

		m_pPeer = nullptr;
		m_pSub = nullptr;
	}
	bool ReadToken(char* token, CBuffer& buf) noexcept
	{
		char* pw = token;
		char ch;

		while (1)
		{
		skipwhite:
			do { ch = buf.getc(); } while (ch > 0 && ch < 0x80 && isspace(ch));	// UTF-8

			if (ch == '/')
			{
				ch = buf.getc();

				if (ch == '/')
				{
					do { ch = buf.getc(); } while (ch != '\n');

					goto skipwhite;
				}
			}

			if (ch == '{' || ch == '}')
			{
				pw[0] = ch;
				pw[1] = 0;

				return true;
			}

			if (ch == '"')
			{
				do { *pw = buf.getc(); } while (*(pw++) != '"');

				*(--pw) = 0;

				return true;
			}

			if (ch == '\0')
			{
				*pw = 0;

				return false;
			}
		}
	}

	char* m_pszName{ nullptr };

	char* m_pszValue{ nullptr };
	double m_flValue{ 0.0 };

	ValveKeyValues* m_pPeer{ nullptr };
	ValveKeyValues* m_pSub{ nullptr };
};
