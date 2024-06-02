#include "Precompiled.hpp"
#include "UtlString.hpp"
#include "UtlCharConv.hpp"

using namespace Hydrogenium::String::Functors;
using namespace Hydrogenium::String;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;


namespace Hydrogenium::String::UnitTest
{
	extern void UnitTest_StrFry();	// Run-time only.
	extern void UnitTest_StrTok();	// Run-time only.
}

extern void UnitTest_Runtime();
extern void UnitTest_UtlString_PerformanceTest() noexcept;

static constexpr bool UnitTest_Trim() noexcept
{
	static_assert(UTIL_Trim<char>("").empty());
	static_assert(UTIL_Trim<char>(" \r\n\t").empty());
	static_assert(UTIL_Trim<char>(" abc ") == "abc");
	static_assert(UTIL_Trim<char>(" abc") == "abc");
	static_assert(UTIL_Trim<char>("abc ") == "abc");
	static_assert(UTIL_Trim<char>("abc") == "abc");

	static_assert(UTIL_Trim<wchar_t>(L"").empty());
	static_assert(UTIL_Trim<wchar_t>(L" \r\n\t").empty());
	static_assert(UTIL_Trim<wchar_t>(L" abc ") == L"abc");
	static_assert(UTIL_Trim<wchar_t>(L" abc") == L"abc");
	static_assert(UTIL_Trim<wchar_t>(L"abc ") == L"abc");
	static_assert(UTIL_Trim<wchar_t>(L"abc") == L"abc");

	return true;
}
static_assert(UnitTest_Trim());



int main(int, char* []) noexcept
{
	using namespace Hydrogenium::String::UnitTest;

	UnitTest_StrFry();	// Run-time only.
	UnitTest_StrTok();	// Run-time only.

	//UnitTest_Runtime();
}
