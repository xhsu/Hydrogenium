// BuildCounter.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#ifdef __INTELLISENSE__
#define _MSVC_TESTING_NVCC
#include <__msvc_all_public_headers.hpp>
#undef _MSVC_TESTING_NVCC
#else
import std.compat;
inline constexpr auto SEEK_CUR = 1;
inline constexpr auto SEEK_END = 2;
inline constexpr auto SEEK_SET = 0;
#endif

#pragma warning(disable : 6385)
#pragma warning(disable : 6386)

inline constexpr char CounterFile[] = SOLUTION_DIR u8"BuildCount.h";

int main(int, char*[]) noexcept
{
	if (auto f = fopen(CounterFile, "rb"); f != nullptr)
	{
		fseek(f, 0, SEEK_END);
		auto const len = ftell(f);

		auto const p = new char[len + 1] {};
		fseek(f, 0, SEEK_SET);
		fread(p, sizeof(char), len, f);

		std::string_view const CounterDeclr{ p, (size_t)len };

		auto const pos = CounterDeclr.find_first_of('\t');
		if (pos == CounterDeclr.npos)
		{
			std::print("Error: Format error on '{}'. This file must be generate by this program.\n", CounterFile);
			return 2;
		}

		auto const pos2 = CounterDeclr.find_first_not_of("0123456789", pos + 1);
		auto const NumberStr = CounterDeclr.substr(pos + 1, pos2 - pos - 1);

		uint32_t iCounter = 0;
		std::from_chars(NumberStr.data(), NumberStr.data() + NumberStr.length(), iCounter);
		std::println("Current counter: '{}'", iCounter);

		fclose(f);
		f = fopen(CounterFile, "wt");
		if (f == nullptr)
		{
			std::print("Error: Cannot write to file '{}'.\n", CounterFile);
			return 3;
		}

		std::print(f, "#define BUILD_COUNT\t{}\n", ++iCounter);

		delete[] p;
		fclose(f);
	}
	else if (f = fopen(CounterFile, "wt"); f != nullptr)
	{
		std::print(f, "#define BUILD_COUNT\t{}\n", 1);
		fclose(f);
	}
	else
	{
		std::print("Error: No access to file '{}'", CounterFile);
		return 1;
	}

	return 0;
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
