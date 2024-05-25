#include "Precompiled.hpp"
#include "UtlString.hpp"
#include "UtlCharConv.hpp"

using namespace std;
using namespace Hydrogenium::String;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;


namespace Hydrogenium::String::UnitTest
{
	extern void UnitTest_StrFry();	// Run-time only.
	extern void UnitTest_StrTok();	// Run-time only.
}

extern void UnitTest_Runtime();



int main(int, char* []) noexcept
{
	using namespace Hydrogenium::String::UnitTest;

	UnitTest_StrFry();	// Run-time only.
	UnitTest_StrTok();	// Run-time only.

	//UnitTest_Runtime();
	fmt::print("{}\n", UTIL_strfromf(0.1));
}
