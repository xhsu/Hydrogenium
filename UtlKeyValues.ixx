module;

// C
#include <cassert>	// assert

// stdio.h
#define SEEK_CUR    1
#define SEEK_END    2
#define SEEK_SET    0

#include <fmt/ranges.h>	// fmt::join

// Still bugged as per May 31 2025.
// #MSVC_BUG_GENERATOR https://developercommunity.visualstudio.com/t/Coroutine-compilation-resulting-in-erro/1510427
#include <experimental/generator>

export module UtlKeyValues;

import std;

// Friendly modules
import UtlConcepts;
import UtlString;

using std::string;
using std::string_view;
using namespace std::literals;

using Value_t = double;

using namespace std::string_literals;
using namespace std::string_view_literals;

static std::experimental::generator<std::string_view> TokenGenerator(std::string_view StrV)
{
	for (std::size_t iCur = 0, iSegmentEnds = 0, iSize = StrV.size();
		iCur < iSize && iSegmentEnds < iSize;
		/* Do Nothing */)
	{
		for (; iCur < iSize && std::isspace(StrV[iCur]); ++iCur) {}	// L Trim

		if (iCur >= iSize)
			co_return;	// END

		if (iCur + 1 < iSize && StrV[iCur] == '/' && StrV[iCur + 1] == '/')	// Handle comment line.
		{
			if (iCur = StrV.find_first_of('\n', iCur); iCur++ == StrV.npos)
				co_return;
			else
				continue;
		}

		if (StrV[iCur] == '{' || StrV[iCur] == '}')
		{
			co_yield StrV.substr(iCur, 1);
			++iCur;
			continue;
		}

		if (iCur = StrV.find_first_of('"', iCur); iCur == StrV.npos)
			co_return;

		++iCur;	// Skip the " symbol itself.

		for (auto iSearchPos = iCur; iSearchPos < iSize; ++iSearchPos)	// Escape supported. But only for \" and not for \\"
		{
			if (iSegmentEnds = StrV.find_first_of('"', iSearchPos); iSegmentEnds == StrV.npos)
				co_return;

			if (StrV[iSegmentEnds - 1] == '\\')
				continue;

			break;
		}

		assert(iCur < iSegmentEnds);

		co_yield StrV.substr(iCur, iSegmentEnds - iCur);

		iCur = iSegmentEnds + 1;
	}

	co_return;
}

static auto TokenFountain(std::experimental::generator<std::string_view>&& GenObj) noexcept
{
	return [GenObj = std::move(GenObj), iter = GenObj.begin(), Sentinel = GenObj.end()]() mutable noexcept -> std::string_view
	{
		if (iter != Sentinel)
		{
			auto str = *iter;
			++iter;
			return str;
		}
		else
			return "";
	};
}

// Entry - Subkey or KeyValue
// Subkey - Contains only Entries, but no value for itself.
// KeyValue - A pair of string.
// Key - the "Name" of a Subkey.
// Value - the "Number/String" part of a KeyValue.

template <typename T>
concept string_pair = pair_like<T> && std::convertible_to<std::tuple_element_t<0, T>, string_view> && std::convertible_to<std::tuple_element_t<1, T>, string_view>;

export struct ValveKeyValues
{
	// Constructors
	ValveKeyValues(void) noexcept = default;
	ValveKeyValues(string_view szName, string_view szValue) noexcept : m_szName(szName), m_szValue(szValue)
	{
		switch (UTIL_GetStringType(m_szValue.c_str()))
		{
		case 1:
		case 2:
			m_flValue = UTIL_StrToNum<Value_t>(m_szValue);
			break;

		case 0:
		default:
			m_flValue = std::numeric_limits<Value_t>::quiet_NaN();
			break;
		}

		UTIL_ReplaceAll(&m_szValue, "\\\""sv, "\""sv);
	}
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
	explicit ValveKeyValues(string_pair auto const& Pair) noexcept : ValveKeyValues(std::get<0>(Pair), std::get<1>(Pair)) {}
	virtual ~ValveKeyValues(void) noexcept { Purge(); }

	ValveKeyValues(const ValveKeyValues&) = delete;
	ValveKeyValues(ValveKeyValues&&) = delete;
	ValveKeyValues& operator=(const ValveKeyValues&) = delete;
	ValveKeyValues& operator=(ValveKeyValues&&) = delete;

	// set/get name
	decltype(auto) Name(this auto&& self) noexcept { return self.m_szName; }

	// load/save file
	bool LoadFromFile(const char* pszFile) noexcept
	{
		[[likely]]
		if (auto f = fopen(pszFile, "rb"); f)
		{
			fseek(f, 0, SEEK_END);

			auto const iFileSize = ftell(f);
			char* p = (char*)calloc(iFileSize + 1, sizeof(char));

			fseek(f, 0, SEEK_SET);
			fread(p, sizeof(char), iFileSize, f);

			string_view StrV(p, p + iFileSize);
			auto fnFountain = TokenFountain(TokenGenerator(StrV));

			Purge();	// Remove everything of current vkv. (Release memoory)
			m_szName = fnFountain();	// First token must be the name of the head vkv.

			[[likely]]
			if (auto szSecondToken = fnFountain(); szSecondToken != "")
			{
				[[unlikely]]
				if (szSecondToken != "{")	// this file contains only one kv pair.
				{
					SetValue(nullptr, szSecondToken);
					return true;
				}
			}
			else
				return false;

			RecursiveRead(fnFountain);

			free(p); p = nullptr;
			fclose(f); f = nullptr;

			return true;
		}

		return false;
	}
	bool SaveToFile(const char* resourceName) noexcept
	{
		[[likely]]
		if (auto f = fopen(resourceName, "wb"); f)
		{
			RecursiveSave(f);

			fclose(f); f = nullptr;
			return true;
		}

		return false;
	}

	// Print to console
	void PrintC(unsigned char iIndent = 0) const noexcept
	{
		auto const iSpaceCountForThisSubkey = GetSpaceCountBtnAllSubKeyValues();

		std::print("{0}\"{1}\"\n{0}{{\n", string(iIndent, '\t'), m_szName);

		for (auto&& obj : *this)
		{
			if (obj.IsKeyValue())
			{
				std::print(
					"{0}{1:<{3}}\"{2}\"\n",
					string(iIndent + 1, '\t'),
					std::format("\"{}\"", obj.m_szName), obj.m_szValue,
					iSpaceCountForThisSubkey
				);
			}
			else if (obj.IsSubkey())
				obj.PrintC(iIndent + 1);
			else
				std::unreachable();
		}

		std::print("{}}}\n", string(iIndent, '\t'));
	}

	// find or create an entry. Only creates an entry if named one cannot be found.
	ValveKeyValues const* FindEntry(const char* pszName) const noexcept
	{
		if (!pszName || !pszName[0])
			return this;

		ValveKeyValues* dat = nullptr;

		for (dat = m_pSub; dat != nullptr; dat = dat->m_pPeer)
		{
			if (dat->m_szName == pszName)
				break;
		}

		return dat;
	}
	ValveKeyValues* AccessEntry(const char* pszName) noexcept	// #UPDATE_AT_CPP23 assume never nullptr.
	{
		if (!pszName || !pszName[0])
			return this;

		ValveKeyValues* dat = nullptr;
		ValveKeyValues* lastItem = nullptr;

		for (dat = GetFirstEntry(); dat; dat = dat->GetNextEntry())
		{
			lastItem = dat;	// 'lastItem' would remain last item if dat becomes null in for check.

			if (dat->m_szName == pszName)
				break;
		}

		// Create one on no-found.
		if (!dat)
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
	ValveKeyValues *GetFirstEntry(void) const noexcept { return m_pSub; }
	ValveKeyValues *GetNextEntry(void) const noexcept { return m_pPeer; }
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
		return !m_szValue.empty();
	}
	inline bool IsSubkey(void) const noexcept
	{
		return m_pSub != nullptr;
	}

	// value, nullptr if inquerying self.
	template<typename T> T GetValue(const char* pszSubkeyName = nullptr, const std::type_identity_t<T>& DefValue = T {}) const noexcept	// Type identity: disable template argument deduction.
	{
		auto dat = FindEntry(pszSubkeyName);

		[[unlikely]]
		if (!dat || dat->m_szValue.empty())
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
				return UTIL_SplitIntoNums<ElemTy>(dat->m_szValue, " \f\n\r\t\v\0") | std::ranges::to<T>();
			else
				return UTIL_Split(dat->m_szValue, " \f\n\r\t\v\0") | std::ranges::to<T>();
		}
		else if constexpr (std::ranges::range<T>)
		{
			if (dat->m_szValue.empty())
				return DefValue;

			T ret{};
			using ElemTy = std::ranges::range_value_t<T>;

			if constexpr (Arithmetic<ElemTy>)
			{
				// It must be a reference, otherwise the result would not be saved into ret.
				for (auto&& [Ref, Val] : std::views::zip(ret, UTIL_SplitIntoNums<ElemTy>(dat->m_szValue, " \f\n\r\t\v\0")))
					Ref = Val;
			}
			else if constexpr (std::convertible_to<string_view, ElemTy>)
			{
				for (auto&& [Ref, Val] : std::views::zip(ret, UTIL_Split(dat->m_szValue, " \f\n\r\t\v\0")))
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
	template<typename... Tys> auto GetValue(const char* pszSubkeyName = nullptr) const noexcept requires(sizeof...(Tys) > 1)
	{
		static constexpr auto RetTypeDeducer = [](void) consteval noexcept
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

		auto dat = FindEntry(pszSubkeyName);
		if (!dat || dat->m_szValue.empty()) [[unlikely]]
			return Ret;

		// The whold complex factory shenanigan is due to the one loss of regular views::take
		// Method provided by: https://stackoverflow.com/q/73207032/15860107

		static auto const fnTakeAdaptor = []<typename GeneratorTy>(GeneratorTy && gen) noexcept requires(requires{ typename GeneratorTy::iterator::value_type; })
		{
			return [gen = std::move(gen)](std::ptrdiff_t iTakeCount) mutable noexcept
				-> std::generator<typename GeneratorTy::iterator::value_type>
			{
				decltype(iTakeCount) i = 0;

				if (not (i++ < iTakeCount))
					co_return;

				for (auto&& e : gen)	// #POTENTIAL_BUG multiple begin() call on the same generator would be UB.
				{
					co_yield e;

					if (not (i++ < iTakeCount))
						break;
				}
			};
		};
		auto Take = fnTakeAdaptor(UTIL_Split(dat->m_szValue, " \f\n\r\t\v\0"));
		static auto const fnAssign = [&Take]<typename T>(T & Output) noexcept
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
					for (auto&& [Ref, Val] : std::views::zip(Output, Take(DIST) | std::views::transform(UTIL_StrToNum<ElemTy>)))
						Ref = Val;
				}
				else if constexpr (std::convertible_to<ElemTy, string_view>)
				{
					for (auto&& [Ref, Val] : std::views::zip(Output, Take(DIST)))
						Ref = static_cast<ElemTy>(Val);
				}
				else
					static_assert(!sizeof(ElemTy), "Unsupported elem of range.");
			}
			else
				static_assert(!sizeof(T), "Unsupported output type.");
		};
		static auto const fnHandle = []<std::size_t... I>(auto & tpl, std::index_sequence<I...>&&) noexcept
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
		ValveKeyValues* dat = AccessEntry(pszSubkeyName);
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

			if (dat->m_szValue.contains('"'))
				UTIL_ReplaceAll(&dat->m_szValue, "\\\"", "\"");

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
	template<typename... Tys> bool SetValue(const char* pszSubkeyName, Tys&&... Values) noexcept requires(sizeof...(Values) > 1)	// Create new entry on no found.
	{
		ValveKeyValues* dat = AccessEntry(pszSubkeyName);
		if (!dat)
			return false;

		static auto constexpr fnFetch = []<typename T>(T&& Param) constexpr noexcept -> decltype(auto)
		{
			if constexpr (std::convertible_to<T, string_view>)	// It's not a dupe of the 'else' case. strings are all range, it would fall into the std::join case if not escape here.
				return std::forward<T>(Param);	// #INVESTIGATE #POTENTIAL_BUG should we return a std::forward thing here? ownership??
			else if constexpr (requires { fmt::join(std::forward<T>(Param), " "); })
				return fmt::join(std::forward<T>(Param), " "); // #FIXME_UNKNOWN_BUG This would lead to the requirement of including fmt/ranges.h in imported source file.
			else if constexpr (pair_like<T>)
				return fmt::join(std::tuple{ Param }, " ");	// Well, since {fmt} doesn't support pair itself...
			else
				return std::forward<T>(Param);
		};

		// Have to convert like this.
		static constexpr string_view szFormatter = UTIL_SpacedFormatter<sizeof...(Values)>();

		dat->m_szValue = std::format(szFormatter, fnFetch(std::forward<Tys>(Values))...);
		dat->m_flValue = std::numeric_limits<Value_t>::quiet_NaN();

		if (dat->m_szValue.contains('"'))
			UTIL_ReplaceAll(&dat->m_szValue, "\\\""sv, "\""sv);

		return true;
	}

	// remove all key/value (except name)
	void Clear(void) noexcept
	{
		if (m_pSub)
			delete m_pSub;
		m_pSub = nullptr;

		m_szValue.clear();
		m_flValue = std::numeric_limits<Value_t>::quiet_NaN();
	}

	// iterator support.
	struct iterator final
	{
		iterator(ValveKeyValues *p) noexcept : m_p(p) {}
		iterator(const iterator &rhs) noexcept : m_p(rhs.m_p) {}
		iterator(iterator &&rhs) noexcept : m_p(rhs.m_p) {}
		iterator &operator=(const iterator &rhs) noexcept { m_p = rhs.m_p; return *this; }
		iterator &operator=(iterator &&rhs) noexcept { m_p = rhs.m_p; return *this; }
		~iterator() noexcept = default;

		iterator &operator++(void) noexcept { m_p = m_p->GetNextEntry(); return *this; }
		ValveKeyValues &operator*(void) noexcept { return *m_p; }
		ValveKeyValues *operator->(void) noexcept { return m_p; }

		constexpr bool operator== (const iterator &rhs) const noexcept { return m_p == rhs.m_p; }

		ValveKeyValues *m_p = nullptr;
	};

	struct const_iterator final
	{
		const_iterator(const ValveKeyValues *p) noexcept : m_p(p) {}
		const_iterator(const iterator &rhs) noexcept : m_p(rhs.m_p) {}
		const_iterator(const const_iterator &rhs) noexcept : m_p(rhs.m_p) {}
		const_iterator(const_iterator &&rhs) noexcept : m_p(rhs.m_p) {}
		const_iterator &operator=(const iterator &rhs) noexcept { m_p = rhs.m_p; return *this; }
		const_iterator &operator=(const const_iterator &rhs) noexcept { m_p = rhs.m_p; return *this; }
		const_iterator &operator=(const_iterator &&rhs) noexcept { m_p = rhs.m_p; return *this; }
		~const_iterator() noexcept = default;

		const_iterator &operator++(void) noexcept { m_p = m_p->GetNextEntry(); return *this; }
		const ValveKeyValues &operator*(void) const noexcept { return *m_p; }
		const ValveKeyValues *operator->(void) const noexcept { return m_p; }

		constexpr bool operator== (const const_iterator &rhs) const noexcept { return m_p == rhs.m_p; }

		const ValveKeyValues *m_p = nullptr;
	};

	inline iterator begin(void) noexcept { return iterator(GetFirstEntry()); }
	inline const_iterator begin(void) const noexcept { return const_iterator(GetFirstEntry()); }
	inline iterator end(void) noexcept { return iterator(nullptr); }
	inline const_iterator end(void) const noexcept { return const_iterator(nullptr); }

private:
	void Purge(void) noexcept
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

	void RecursiveRead(auto&& fnFountain) noexcept requires(std::convertible_to<std::invoke_result_t<decltype(fnFountain)>, string_view>)
	{
		for (auto szToken = fnFountain(); szToken != ""; szToken = fnFountain())
		{
			if (szToken == "}")
			{
				return;
			}
			else if (auto szToken2 = fnFountain(); szToken2 != "")
			{
				if (szToken2 == "{")
				{
					auto p2 = new ValveKeyValues(szToken);
					EnrollEntry(p2);
					p2->RecursiveRead(fnFountain);
				}
				else
					EnrollEntry(new ValveKeyValues(szToken, szToken2));
			}
		}
	}
	void RecursiveSave(FILE* f, std::size_t iIndent = 0) noexcept
	{
		auto const iSpaceCountForThisSubkey = GetSpaceCountBtnAllSubKeyValues();

		std::print(f, "{0}\"{1}\"\n{0}{{\n", string(iIndent, '\t'), m_szName);

		for (auto pCur = GetFirstEntry(); pCur; pCur = pCur->GetNextEntry())
		{
			if (pCur->IsKeyValue())
			{
				// Since we stripped the \" at the load-in stage, we have to 'restore it' when saving.
				if (pCur->m_szValue.contains('"'))
				{
					string szDup = pCur->m_szValue;
					UTIL_ReplaceAll(&szDup, "\""sv, "\\\""sv);

					std::print(f,
						"{0}{1:<{3}}\"{2}\"\n",
						string(iIndent + 1, '\t'),	// Pre-indent
						std::format("\"{}\"", pCur->m_szName), szDup,	// KV
						iSpaceCountForThisSubkey
					);
				}
				else
				{
					std::print(f,
						"{0}{1:<{3}}\"{2}\"\n",
						string(iIndent + 1, '\t'),	// Pre-indent
						std::format("\"{}\"", pCur->m_szName), pCur->m_szValue,	// KV
						iSpaceCountForThisSubkey
					);
				}
			}
			else if (pCur->IsSubkey())
				pCur->RecursiveSave(f, iIndent + 1);
			else
				std::unreachable();
		}

		std::print(f, "{}}}\n", string(iIndent, '\t'));
	}

	std::size_t GetIndentCountBetweenKeyAndValue(int iBlockAlignedCharCount) const noexcept
	{
		int const iTotalKeyStrlen = (int)m_szName.length() + 2;	// Plus "" on both side.
		int iDelta = iBlockAlignedCharCount - iTotalKeyStrlen;	// Have to be signed.

		assert(iDelta >= 0);

		int iIndentCount = 0;
		for (; iDelta > 0; iDelta -= 4)
			++iIndentCount;

		return std::max(iIndentCount, 1);
	}
	std::size_t GetSpaceCountBtnAllSubKeyValues(void) const noexcept
	{
		if (!IsSubkey())
			return 0;

		std::size_t iMaxLen = 2;
		for (auto pCur = GetFirstKeyValue(); pCur; pCur = pCur->GetNextKeyValue())
		{
			if (pCur->m_szName.length() > iMaxLen)
				iMaxLen = pCur->m_szName.length();
		}

		return iMaxLen + 3;	// 2 for "" and 1 for at least one addition space.
	}

private:
	string m_szName{};

	string m_szValue{};
	Value_t m_flValue = std::numeric_limits<Value_t>::quiet_NaN();

	ValveKeyValues* m_pPeer = nullptr;
	ValveKeyValues* m_pSub = nullptr;
};

export
constexpr bool operator==(const ValveKeyValues::iterator &lhs, const ValveKeyValues::const_iterator &rhs) noexcept { return lhs.m_p == rhs.m_p; }
