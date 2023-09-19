// Linear Algebra.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "../UtlLinearAlgebra.hpp"

int main() noexcept
{
	using namespace Hydrogenium;

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
	static_assert((2 * Vector2::I() + 4 * Vector2::J()) / 2 == vecTwo);
	static_assert(2 * Vector{ -1, -2 } == Vector{ -1, -2 } * 2);

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

	// Property member
	static_assert(vecFive1.x == 1 && vecFive1.y == 2 && vecFive1.z == 3 && vecFive1.w == 4);

	// Dot product
	static_assert(DotProduct(Vector2::I(), Vector2::J()) == 0);
	static_assert(DotProduct(Vector{ 1, 2 }, Vector{ 3, 4 }) == 11);

	// Cross product
	static_assert(CrossProduct(Vector3::I(), Vector3::J()) == Vector3::K());
	static_assert(CrossProduct(Vector{ 1, 2, 3 }, Vector{ 4, 5, 6 }) == Vector{ -3, 6, -3 });
	static_assert(CrossProduct(Vector{ 1, 2 }, Vector{ 3, 4 }) == Vector{ 0, 0, -2 });

	// Angle between
	assert((Vector2::I() ^ Vector2::J()) == 90);
	assert((Vector3::J() ^ Vector3::K()) == 90);
	assert(std::abs((Vector{ 1, 2 } ^ Vector{ 3, 4 }) - 10.30484) < 1e-5);

	// Vector2 Specialized
	assert(Vector2::I().Angle() == 0);
	assert(std::abs(Vector2::J().Angle() - 90) < 1e-5);
	assert(std::abs(Vector{ 1, 1 }.Angle() - 45) < 1e-5);
	assert(std::abs(Vector{ 3, 4 }.Angle() - 53.130102) < 1e-5);	// 3-4-5 triangle

	assert(Vector2::I().Rotate(90) == Vector2::J());
	assert((Vector{ 1, 1 }.Rotate(90) == Vector{ -1, 1 }));
	assert((Vector{ 1, 1 }.Rotate(45) == Vector{ 0, std::numbers::sqrt2 }));

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
