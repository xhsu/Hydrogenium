#include <assert.h>

#include <print>
#include <source_location>
#include <string_view>
#include <type_traits>

template <auto iEnum, typename T = decltype(iEnum)> requires std::is_enum_v<T>
struct reflexpr
{
	template <T iEnumFwd = iEnum>
	static consteval std::string_view _Impl_GetEnumerator() noexcept
	{
		std::string_view const szFuncName = "_Impl_GetEnumerator";
		std::string_view const szFuncSig = std::source_location::current().function_name();
		auto const src = szFuncSig.find(szFuncName) + szFuncName.length() + 1;
		auto const last = szFuncSig.find_last_of('>');

		if (auto const ret = szFuncSig.substr(src, last - src); !ret.contains(':'))
		{
			return ret;
		}
		else
		{
			return ret.substr(ret.find_last_of(':') + 1);
		}
	}

	template <T iEnumFwd = iEnum>
	static consteval std::string_view _Impl_GetEnumeration() noexcept
	{
		std::string_view const szFuncName = "_Impl_GetEnumeration";
		std::string_view const szFuncSig = std::source_location::current().function_name();

		auto const src = szFuncSig.find("enum ") + 5;
		auto const last = szFuncSig.find_last_of('>', szFuncSig.find(szFuncName));

		return szFuncSig.substr(src, last - src);
	}

	inline static constexpr std::string_view enumerator = _Impl_GetEnumerator();
	inline static constexpr std::string_view enumeration = _Impl_GetEnumeration();
};

enum MyMuggleEnum : __int8
{
	ITEM1,
	ITEM2,
};

enum struct MyScopedEnum : __int16
{
	ITEM3 = 3,
	ITEM4,
};

void UnitTest_Reflexpr(void) noexcept
{
	std::print("enumerator: '{}'\n", reflexpr<MyMuggleEnum::ITEM1>::enumerator);
	std::print("enumeration: '{}'\n", reflexpr<MyMuggleEnum::ITEM1>::enumeration);
	std::print("enumerator: '{}'\n", reflexpr<MyScopedEnum::ITEM4>::enumerator);
	std::print("enumeration: '{}'\n", reflexpr<MyScopedEnum::ITEM4>::enumeration);
}
