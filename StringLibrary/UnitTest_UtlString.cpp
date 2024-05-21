#include "Precompiled.hpp"
#include "UtlString.hpp"

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




constexpr double UTIL_atof(const char* s) noexcept
{
	constexpr auto is_digit =
		[](auto c) noexcept
		{
			return '0' <= c && c <= '9';
		};

	// This function stolen from either Rolf Neugebauer or Andrew Tolmach. 
	// Probably Rolf.
	double a{};
	int32_t e{}, c{};
	bool neg = false;

	if (s && *s == '-')
	{
		++s;
		neg = true;
	}

	while ((c = *s++) != '\0' && is_digit(c))
	{
		a = a * 10.0 + (c - '0');
	}

	if (c == '.')
	{
		while ((c = *s++) != '\0' && is_digit(c))
		{
			a = a * 10.0 + (c - '0');
			e = e - 1;
		}
	}

	if (c == 'e' || c == 'E')
	{
		int32_t sign = 1;
		int32_t i = 0;

		c = *s++;

		if (c == '+')
		{
			c = *s++;
		}
		else if (c == '-')
		{
			c = *s++;
			sign = -1;
		}

		while (is_digit(c))
		{
			i = i * 10 + (c - '0');
			c = *s++;
		}
		e += i * sign;
	}

	while (e > 0)
	{
		a *= 10.0;
		--e;
	}

	while (e < 0)
	{
		a *= 0.1;
		++e;
	}

	return neg ? -a : a;
}





int main(int, char* []) noexcept
{
	using namespace Hydrogenium::String::UnitTest;

	UnitTest_StrFry();	// Run-time only.
	UnitTest_StrTok();	// Run-time only.

	//UnitTest_Runtime();
}
