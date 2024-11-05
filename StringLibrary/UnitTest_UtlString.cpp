#include "Precompiled.hpp"
#include "UtlString.hpp"
#include "UtlCharConv.hpp"

using namespace Hydrogenium::String::Components;
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



int main(int, char* []) noexcept
{
	using namespace Hydrogenium::String::UnitTest;

	UnitTest_StrFry();	// Run-time only.
	UnitTest_StrTok();	// Run-time only.

	//UnitTest_Runtime();
	UnitTest_UtlString_PerformanceTest();
}

/*
Fwd Optimized - 0.18820117416381835μs
Non-Optimized - 0.1688611222267151μs
non-opt/opt == 89.7%
Average time: 197.357s

Fwd Optimized - 0.06444949893951415μs
Non-Optimized - 0.06908581686019898μs
non-opt/opt == 107.2%
Average time: 72.452s

// Pointer version - release
Fwd Optimized - 0.07568526058197021μs
Non-Optimized - 0.04803600950241089μs
non-opt/opt == 63.5%
Average time: 79.370s
*/
