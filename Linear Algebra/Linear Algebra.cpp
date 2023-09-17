// Linear Algebra.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <typeindex>
#include <typeinfo>

#include "../UtlLinearAlgebra.hpp"

using namespace Hydrogenium;

int main() noexcept
{
	//static_assert(std::is_trivial_v<Vector2>);
	static_assert(std::is_standard_layout_v<Vector2>);
	static_assert(std::ranges::input_range<Vector2>);
	static_assert(std::ranges::output_range<Vector2, vec_t>);
	static_assert(std::is_same_v<std::ranges::range_value_t<Vector2>, vec_t>);

	using std::array;
	static_assert(sizeof(Vector2) == 2 * sizeof(vec_t));

	// Construction
	static constexpr array rgiArr{ 1, 2, 3, 4 };
	static constexpr Vector2 vecTwo{ 1, 2 }, vecTwo1{ rgiArr }, vecTwo2{ rgiArr, 5 };
	static constexpr Vector<5> vecFive{ rgiArr }, vecFive1{ rgiArr, 5 };

	// Math Op: EQUALITY
	static_assert(Vector2{} == Vector2::Zero());
	static_assert(vecTwo == vecTwo1 && vecTwo1 == vecTwo2);
	static_assert(vecFive != vecFive1);
	static_assert(Vector2::I() != Vector2::J());

	// Math Op: Compare
	static_assert(vecTwo < Vector2{ 2, 2 });
	static_assert(vecTwo <= Vector2{ 2, 2 } && vecTwo <= vecTwo1);
	static_assert(vecTwo > Vector2{ 1, 1 });
	static_assert(vecTwo >= Vector2{ 1, 1 } && vecTwo >= vecTwo1);

	// C++ Op: Indexing & Iterating
	static_assert(vecTwo[0] == vecTwo1[0] && vecTwo[1] == vecTwo1[1]);
	static_assert(std::ranges::equal(
		vecTwo.begin(), vecTwo.end(),
		vecTwo1.begin(), vecTwo1.end()
	));
	static_assert(std::ranges::equal(
		vecTwo.rbegin(), vecTwo.rend(),
		vecTwo1.rbegin(), vecTwo1.rend()
	));
	static_assert(std::ranges::equal(
		vecTwo.cbegin(), vecTwo.cend(),
		vecTwo1.cbegin(), vecTwo1.cend()
	));
	static_assert(std::ranges::equal(
		vecTwo.crbegin(), vecTwo.crend(),
		vecTwo1.crbegin(), vecTwo1.crend()
	));

	Vector2 vecDynTwo{ vecTwo };
	static_assert(std::is_same_v<decltype(vecDynTwo[0]), vec_t&>);
	static_assert(std::is_same_v<decltype(vecTwo[0]), vec_t const&>);
	assert(vecDynTwo[0] == 1);
	vecDynTwo[0] = 777;
	assert(vecDynTwo[0] == 777);
	vecDynTwo[0] = 1;
	assert(vecDynTwo == vecTwo);

	// Math Op: Length
	auto const flRootFive = std::sqrt(5.0);
	assert(vecTwo.magnitude == flRootFive);
	vecDynTwo.magnitude = 1;
	assert(vecDynTwo == vecTwo.direction);
	vecDynTwo.magnitude = flRootFive;
	assert(vecDynTwo == vecTwo);

	// Math Op: Direction
	assert(vecDynTwo.direction == vecTwo.WithLengthOf(1));
	vecDynTwo.direction = Vector2::I();
	assert(std::abs(flRootFive - vecDynTwo.magnitude) < Hydrogenium::VEC_EPSILON);
	vecDynTwo.direction = vecTwo;
	assert(std::abs(flRootFive - vecDynTwo.magnitude) < Hydrogenium::VEC_EPSILON);
	assert(vecDynTwo == vecTwo);
	assert(Vector2::I().WithDirOf(Vector2::J()) == Vector2::J());
	assert(std::abs(vecDynTwo.WithDirOf(Vector2::J()).magnitude - flRootFive) < Hydrogenium::VEC_EPSILON);

	// Math Op: Algebra
	static_assert(Vector2::I() + Vector2::J() == Vector2{ 1, 1 });
	static_assert(vecTwo - Vector2::I() - Vector2{ 0, 2 } == Vector2::Zero());
	static_assert((Vector2::I() * 2 + Vector2::J() * 4) / 2 == vecTwo);

	// C++ Op: Copy to & casting
	vecDynTwo.fill(7);
	assert(vecDynTwo == Vector2(7, 7));
	vecDynTwo = vecTwo;

	array<vec_t, 5> rgfl{};
	assert(std::ranges::equal_range(rgfl, 0));
	vecDynTwo.CopyTo(rgfl);
	assert(std::ranges::equal(rgfl, array{ 1.f, 2.f, 0.f, 0.f, 0.f }));
	vecFive1.CopyTo(rgfl);
	assert(std::ranges::equal(rgfl, array{ 1.f, 2.f, 3.f, 4.f, 5.f }));

	static_assert(vecFive1.asVector<2>() == vecTwo);
	static_assert(vecTwo.asVector<3>() == Vector{ 1, 2, 0 });

	// CTAD
	static_assert(Vector{ 1, 2 } == vecTwo);
	static_assert(Vector{ 1, 2, 3, 4, 5 } == vecFive1);

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
