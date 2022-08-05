module;

#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS	// This is a very unsafe module.
#endif

// C++
#include <concepts>	// std::integral, etc...
#include <filesystem>	// std::filesystem::path
#include <limits>	// std::numeric_limit<double>
#include <ranges>
#include <string>	// std::string

// C++ external libs
#include <range/v3/range.hpp>	// #UPDATE_AT_CPP23
#include <range/v3/view.hpp>
#include <experimental/generator>	// #UPDATE_AT_CPP23
#include <fmt/ranges.h>

// C
#include <cassert>	// assert
#include <cctype>	// isspace isdigit
#include <cmath>	// roundf
#include <cstdio>	// fopen fclose fread fwrite

export module UtlKeyValues;

import UtlBuffer;
import UtlConcepts;
import UtlString;

using std::string;
using std::string_view;

using Value_t = double;

using namespace std::string_literals;
using namespace std::string_view_literals;

// Entry - Subkey or KeyValue
// Subkey - Contains only Entries, but no value for itself.
// KeyValue - A pair of string.
// Key - the "Name" of a Subkey.
// Value - the "Number/String" part of a KeyValue.

export struct ValveKeyValues
{
	// Constructors
	ValveKeyValues(void) noexcept = default;
	explicit ValveKeyValues(const char* pszName) noexcept : m_szName(pszName) {}
	explicit ValveKeyValues(string_view szName) noexcept : m_szName(szName) {}
	explicit ValveKeyValues(const std::filesystem::path& hPath) noexcept
	{
#ifdef _DEBUG
		assert(LoadFromFile(hPath.string().c_str()));
#else
		LoadFromFile(hPath.string().c_str());
#endif
	}
	virtual ~ValveKeyValues(void) noexcept
	{
		RemoveEverything();
	}

	// set/get name
	string& Name(void) noexcept { return m_szValue; }	// #UPDATE_AT_CPP23 explict this
	const string& Name(void) const noexcept { return m_szValue; }

	// load/save file
	bool LoadFromFile(const char* resourceName) noexcept
	{
		FILE* f = fopen(resourceName, "rb");
		if (!f)
			return false;

		fseek(f, 0, SEEK_END);
		auto fileSize = ftell(f);
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
		RecursiveSaveToBuffer(buf);

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
				pCurrentKey->Name() = token;
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
		RecursiveSaveToBuffer(buf);

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

			if (dat->m_szName == pszName)
				break;
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
				if (auto val = UTIL_StrToNum<int>(dat->m_szName); index <= val)
					index = val + 1;
			}

			dat = new ValveKeyValues(std::to_string(index).c_str());
		}
		else
			dat = new ValveKeyValues(pszName);

		EnrollEntry(dat);
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
			if (pszName == dat->m_szName)
				break;
		}

		if (!dat)
			return false;

		delete dat;
		return true;
	}

	// add/remove an existing entry. no memory gets deleted or allocated.
	void EnrollEntry(ValveKeyValues* pEntry) noexcept
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
	void DelistEntry(ValveKeyValues* pEntry) noexcept
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
	inline bool IsEmpty(void) const noexcept
	{
		return !m_pSub && m_szValue.empty();
	}
	inline bool IsKeyValue(void) const noexcept
	{
		return m_szValue.empty();
	}
	inline bool IsSubkey(void) const noexcept
	{
		return m_pSub != nullptr;
	}

	// value, nullptr if inquerying self.
	template<typename T> T GetValue(const char* pszSubkeyName = nullptr, const T& DefValue = T {}) noexcept
	{
		ValveKeyValues* dat = FindEntry(pszSubkeyName);
		if (!dat || dat->m_szValue.empty()) [[unlikely]]
		{
			if constexpr (std::convertible_to<T, string> && std::is_pointer_v<T>)
				return DefValue == nullptr ? "" : DefValue;
			else
				return DefValue;
		}

		if constexpr (std::floating_point<T>)
		{
			return static_cast<T>(dat->m_flValue);
		}
		else if constexpr (std::integral<T> || std::is_enum_v<T>)
		{
			return static_cast<T>(std::round(dat->m_flValue));
		}
		else if constexpr (std::convertible_to<T, string_view>)
		{
			if constexpr (std::is_pointer_v<T>)
				return dat->m_szValue.c_str();
			else
				return dat->m_szValue;
		}
		else if constexpr (ResizableContainer<T>)
		{
			if (dat->m_szValue.empty())
				return DefValue;

			using ElemTy = std::ranges::range_value_t<T>;

			if constexpr (Arithmetic<ElemTy>)
				return UTIL_SplitIntoNums<ElemTy>(dat->m_szValue, " \f\n\r\t\v\0") | ::ranges::to<T>;	// #UPDATE_AT_CPP23 ranges::to
			else
				return UTIL_Split(dat->m_szValue, " \f\n\r\t\v\0") | ::ranges::to<T>;	// #UPDATE_AT_CPP23 ranges::to
		}
		else if constexpr (std::ranges::range<T>)
		{
			if (dat->m_szValue.empty())
				return DefValue;

			T ret{};
			using ElemTy = std::ranges::range_value_t<T>;

			// #UPDATE_AT_CPP23 zip it!
			if constexpr (Arithmetic<ElemTy>)
			{
				// It must be a reference, otherwise the result would not be saved into ret.
				for (auto&& [Ref, Val] : ::ranges::zip_view(::ranges::ref_view{ ret }, UTIL_SplitIntoNums<ElemTy>(dat->m_szValue, " \f\n\r\t\v\0")))
					Ref = Val;
			}
			else if constexpr (std::convertible_to<string_view, ElemTy>)
			{
				for (auto&& [Ref, Val] : ::ranges::zip_view(::ranges::ref_view{ ret }, UTIL_Split(dat->m_szValue, " \f\n\r\t\v\0")))
					Ref = static_cast<ElemTy>(Val);
			}
			else if constexpr (tuple_like<ElemTy> || std::ranges::range<T>)
				static_assert(!sizeof(T), "No 2D array or tuple allowed.");
			else
				static_assert(!sizeof(T), "Unsupported elem of range.");

			return ret;
		}
		else if constexpr (tuple_like<T>)
			static_assert(!sizeof(T), "Directly put multiple return types into angled brackets, do not wrap with anything.");
		else
			static_assert(!sizeof(T), "Unsupported type involved.");

		return DefValue;
	}
	template<typename... Tys> auto GetValue(const char* pszSubkeyName = nullptr) noexcept requires(sizeof...(Tys) > 1)
	{
		static constexpr auto RetTypeDeducer = [](void) consteval
		{
			/*
			* This is essentially 
			std::conditional_t<(sizeof...(Tys) > 2), std::tuple<Tys...>, std::pair<Tys...>> Ret{};
			* However, all three template parameters of std::conditional_t are forced evaluated/instantiated,
			therefore things like std::pair<T, U, V> would causes program ill-formed. It doesn't matter that this non-sense 'pair' never put into use.
			*/
			if constexpr (sizeof...(Tys) == 2)
				return std::pair<Tys...>{};
			else
				return std::tuple<Tys...>{};
		};
		decltype(RetTypeDeducer()) Ret{};

		ValveKeyValues* dat = FindEntry(pszSubkeyName);
		if (!dat || dat->m_szValue.empty()) [[unlikely]]
			return Ret;

		// The whold complex factory shenanigan is due to the one loss of regular views::take
		// Method provided by: https://stackoverflow.com/q/73207032/15860107

		static auto const fnTakeFactory = []<typename GeneratorTy>(GeneratorTy && gen) requires(requires{ typename GeneratorTy::iterator::value_type; })
		{
			return [gen = std::move(gen)](std::ptrdiff_t iTakeCount) mutable
				-> std::experimental::generator<typename GeneratorTy::iterator::value_type>
			{
				decltype(iTakeCount) i = 0;

				if (not (i++ < iTakeCount))
					co_return;

				for (auto&& e : gen)
				{
					co_yield e;

					if (not (i++ < iTakeCount))
						break;
				}
			};
		};
		auto Take = fnTakeFactory(UTIL_Split(dat->m_szValue, " \f\n\r\t\v\0"));
		static auto const fnAssign = [&Take]<typename T>(T & Output)
		{
			if constexpr (std::is_arithmetic_v<T>)
			{
				// It actually iterate only once.
				for (auto&& Str : Take(1))
					Output = UTIL_StrToNum<T>(Str);
			}
			else if constexpr (std::is_enum_v<T>)
			{
				for (auto&& Str : Take(1))
					Output = static_cast<T>(UTIL_StrToNum<std::underlying_type_t<T>>(Str));
			}
			else if constexpr (std::convertible_to<T, string_view>)
			{
				for (auto&& Str : Take(1))
					Output = static_cast<T>(Str);
			}
			else if constexpr (std::ranges::range<T>)	// Tuples and pairs won't be supported in this case. It just doesn't make sense.
			{
				using ElemTy = std::ranges::range_value_t<T>;
				auto const DIST = std::ranges::distance(Output);

				assert(DIST > 0);

				if constexpr (std::is_arithmetic_v<ElemTy>)
				{
					for (auto&& [Ref, Val] : ::ranges::zip_view(::ranges::ref_view{ Output }, Take(DIST) | std::views::transform(UTIL_StrToNum<ElemTy>)))
						Ref = Val;
				}
				else if constexpr (std::convertible_to<ElemTy, string_view>)
				{
					for (auto&& [Ref, Val] : ::ranges::zip_view(::ranges::ref_view{ Output }, Take(DIST)))
						Ref = static_cast<ElemTy>(Val);
				}
				else
					static_assert(!sizeof(ElemTy), "Unsupported elem of range.");
			}
			else
				static_assert(!sizeof(T), "Unsupported output type.");
		};
		static auto const fnHandle = []<std::size_t... I>(auto & tpl, std::index_sequence<I...>&&)
		{
			// The existance of this function is because that... fucking C++ does not define the order of function parameter evaluation.
			// Detail explain: https://stackoverflow.com/q/2934904/15860107
			(fnAssign(std::get<I>(tpl)), ...);
		};

		// And it seems like parameter pack cannot unpack in initializer list.
		fnHandle(Ret, std::make_index_sequence<sizeof...(Tys)>{});

		return Ret;
	}
	template<typename T> bool SetValue(const char* pszSubkeyName, const T& Value) noexcept	// Create new entry on no found.
	{
		ValveKeyValues* dat = FindEntry(pszSubkeyName, true);
		if (!dat)
			return false;

		if constexpr (std::is_arithmetic_v<T> || std::is_enum_v<T>)
		{
			dat->m_szValue = std::to_string(Value);
			dat->m_flValue = static_cast<Value_t>(Value);
		}
		else if constexpr (std::convertible_to<T, string_view>)	// Since string is also a type of range, it must place before it. Otherwise the string will be split by ' ' every other letter.
		{
			dat->m_szValue = Value;

			switch (UTIL_GetStringType(dat->m_szValue.c_str()))
			{
			default:
			case 0:	// String
				dat->m_flValue = std::numeric_limits<Value_t>::quiet_NaN();
				break;

			case 1:	// Integer
			case 2:	// Floating point
				dat->m_flValue = UTIL_StrToNum<Value_t>(dat->m_szValue);
				break;
			}
		}
		else if constexpr (std::ranges::range<T> || tuple_like<T>)	// Tuple-like concept? #UPDATE_AT_CPP23 #UPDATE_AT_CPP26
		{
			dat->m_szValue = fmt::format("{}", fmt::join(Value, " "));
			dat->m_flValue = std::numeric_limits<Value_t>::quiet_NaN();
		}
		else
		{
			static_assert(!sizeof(T), "Unsupported type involved.");
		}

		return true;
	}
	template<typename... Tys> bool SetValue(const char* pszSubkeyName, const Tys&... Values) noexcept requires(sizeof...(Values) > 1)	// Create new entry on no found.
	{
		ValveKeyValues* dat = FindEntry(pszSubkeyName, true);
		if (!dat)
			return false;

		static auto constexpr fnFetch = [](auto&& Param) constexpr -> decltype(auto)
		{
			using T = std::decay_t<decltype(Param)>;

			if constexpr (std::convertible_to<T, string_view>)
				return Param;	// #INVESTIGATE why can't I use std::forward here?
			else if constexpr (pair_like<T>)
				return fmt::join(std::tuple{ Param }, " ");	// Well, since {fmt} doesn't support pair itself...
			else if constexpr (std::ranges::range<T> || tuple_like<T>)
				return fmt::join(Param, " ");	// #FIXME_UNKNOWN_BUG This would lead to the requirement of including fmt/ranges.h in imported source file.
			else
				return Param;
		};

		// Have to convert like this.
		static constexpr string_view szFormatter = UTIL_SpacedFormatter<sizeof...(Values)>();

		dat->m_szValue = fmt::format(szFormatter, fnFetch(Values)...);
		dat->m_flValue = std::numeric_limits<Value_t>::quiet_NaN();

		return true;
	}

	// remove all key/value (except name)
	void Clear(void) noexcept
	{
		if (m_pSub)
			delete m_pSub;
		m_pSub = nullptr;

		m_szValue.clear();
		m_flValue = 0;
	}

private:
	void RemoveEverything(void) noexcept
	{
		ValveKeyValues* dat = nullptr;
		ValveKeyValues* datNext = nullptr;

		for (dat = m_pSub; dat != nullptr; dat = datNext)
		{
			datNext = dat->m_pPeer;
			dat->m_pPeer = nullptr;
			delete dat;
		}
		m_pSub = nullptr;

		for (dat = m_pPeer; dat && dat != this; dat = datNext)
		{
			datNext = dat->m_pPeer;
			dat->m_pPeer = nullptr;
			delete dat;
		}
		m_pPeer = nullptr;

		m_flValue = std::numeric_limits<Value_t>::quiet_NaN();

		m_szValue.clear();
		m_szName.clear();
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
					dat->m_flValue = UTIL_StrToNum<Value_t>(token);

				dat->m_szValue = token;
			}
		}
	}
	void RecursiveSaveToBuffer(CBuffer& buf, size_t indentLevel = 0) noexcept
	{
		WriteIndents(buf, indentLevel);
		buf.Write("\"", 1);
		buf.Write(m_szName.c_str(), m_szName.length());
		buf.Write("\"\n", 2);
		WriteIndents(buf, indentLevel);
		buf.Write("{\n", 2);

		int iBlockAlignedCharCount = 0;
		for (ValveKeyValues* dat = GetFirstKeyValue(); dat != nullptr; dat = dat->GetNextKeyValue())	// Skip all subkeys. Only keyvalues involved.
		{
			int iCurTotalStrlen = (int)dat->m_szName.length() + 2;	// Plus "" on both side.

			if (iBlockAlignedCharCount < iCurTotalStrlen)
				iBlockAlignedCharCount = iCurTotalStrlen;
		}

		if ((iBlockAlignedCharCount % 4) == 0)
			++iBlockAlignedCharCount;	// Plus one. This is becuase we have to got at least one indent, the "key" and "value" cannot concatenate with each other.

		while (iBlockAlignedCharCount % 4)	// And then find the next perfect aligned pos.
			iBlockAlignedCharCount++;

		for (ValveKeyValues* dat = m_pSub; dat != nullptr; dat = dat->m_pPeer)
		{
			if (dat->m_pSub)	// This is a Subkey
			{
				dat->RecursiveSaveToBuffer(buf, indentLevel + 1);
			}
			else	// This is a Keyvalue pair.
			{
				WriteIndents(buf, indentLevel + 1);

				// Key
				buf.Write("\"", 1);
				buf.Write(dat->m_szName.c_str(), dat->m_szName.length());
				buf.Write("\"", 1);

				// Indent(s)
				WriteIndents(buf, dat->GetIndentCountBetweenKeyAndValue(iBlockAlignedCharCount));

				// Value
				buf.Write("\"", 1);
				buf.Write(dat->GetValue<const char*>(), strlen(dat->GetValue<const char*>()));
				buf.Write("\"\n", 2);
			}
		}

		WriteIndents(buf, indentLevel);
		buf.Write("}\n", 2);
	}

	size_t GetIndentCountBetweenKeyAndValue(int iBlockAlignedCharCount) noexcept
	{
		int iTotalKeyStrlen = (int)m_szName.length() + 2;	// Plus "" on both side.
		int iDelta = iBlockAlignedCharCount - iTotalKeyStrlen;	// Have to be signed.

		assert(iDelta >= 0);

		int iIndentCount = 0;
		for (; iDelta > 0; iDelta -= 4)
			++iIndentCount;

		return std::max(iIndentCount, 1);
	}
	void WriteIndents(CBuffer& buf, size_t indentLevel) noexcept
	{
		for (size_t i = 0; i < indentLevel; ++i)
		{
			buf.Write("\t", 1);
		}
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
				__int8 iEscapingCount = 0;
			LAB_LOOP:;
				*pw = buf.getc();

				if (*pw == '\\')
					iEscapingCount = iEscapingCount ? 0 : 2;	// Since the escape check is after flag reduction. So there were 2 characters we need to cover. The '\\' itself and the symbol next to it.
				else if (iEscapingCount)
					--iEscapingCount;

				if (iEscapingCount == 2)	// The very '\\' causes escape must be overwrite. It can't be appear in the text.
					goto LAB_LOOP;
				else if (!iEscapingCount && *pw == '"')
					goto LAB_LOOP_END;

				++pw;
				goto LAB_LOOP;

			LAB_LOOP_END:;
				*pw = 0;	// Remove the '"' used in pairing.
				return true;
			}

			if (ch == '\0')
			{
				*pw = 0;

				return false;
			}
		}
	}

private:
	string m_szName{};

	string m_szValue{};
	Value_t m_flValue = 0.0;

	ValveKeyValues* m_pPeer = nullptr;
	ValveKeyValues* m_pSub = nullptr;
};
