// Linear Algebra.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "../UtlLinearAlgebra.hpp"

using namespace Hydrogenium;

int main() noexcept
{
	//static_assert(std::is_trivial_v<Vector2>);
	static_assert(std::is_standard_layout_v<Vector2>);
	//static_assert(std::ranges::range<Vector2>);

	using std::array;
	static_assert(sizeof(Vector2) == 2 * sizeof(vec_t));

	// Construction
	static constexpr array rgiArr{ 1, 2, 3, 4 };
	static constexpr Vector2 vecTwo{ 1, 2 }, vecTwo1{ rgiArr }, vecTwo2{ rgiArr, 5 };
	static constexpr Vector<5> vecFive{ rgiArr }, vecFive1{ rgiArr, 5 };

	// Math operations
	static_assert(vecTwo == vecTwo1 && vecTwo1 == vecTwo2);
	static_assert(vecFive != vecFive1);
	static_assert(Vector2::I() != Vector2::J());

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
