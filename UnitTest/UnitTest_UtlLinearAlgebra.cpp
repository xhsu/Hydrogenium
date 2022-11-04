#include <array>

#include <cassert>

#include <fmt/color.h>
#include <fmt/ranges.h>

#include "..\gcem\include\gcem.hpp"

#include <range/v3/range.hpp>
#include <range/v3/view.hpp>

#include "UnitTest.hpp"

import UtlLinearAlgebra;

void UnitTest_Vector2D(void) noexcept
{
	Log("Starting...");

	//static_assert(std::is_trivial_v<Vector2D>);
	static_assert(std::is_standard_layout_v<Vector2D>);
	static_assert(std::ranges::range<Vector2D>);

	using std::array;
	static_assert(sizeof(Vector2D) == 2 * sizeof(vec_t));

	// Construction
	static constexpr Vector2D vecZero(0, 0);
	static_assert(vecZero == Vector2D::Zero());

	static constexpr Vector2D vecQuad(7);
	static_assert(vecQuad.width == 7 && vecQuad.height == 7);

	static constexpr Vector2D vec1({ 1, 2 }), vec2({ 1, 2, 3 });
	static_assert(vec1 == vec2 && vec1.x == 1 && vec2.y == 2);

	static constexpr double rgdb1[] = { 1, 2 };
	static constexpr unsigned int rgui1[] = { 1, 2, 3 };
	static_assert(Vector2D(rgdb1) == Vector2D(rgui1) && Vector2D(rgdb1) == vec1 && Vector2D(rgui1) == vec2);

	static constexpr array<long double, 2> rgldb1{ 1, 2 };
	static constexpr array<long long, 3> rgll1{ 1, 2, 3 };
	static_assert(Vector2D(rgldb1) == Vector2D(rgll1));

	// Operators
	static_assert(-vec1 == Vector2D{ -1, -2 });
	static_assert(-vec1 != Vector2D{ -1.0f + std::numeric_limits<float>::epsilon(), -2.0f });
	static_assert(vec1 < 3);
	static_assert(vec1 >= Vector2D::J());
	static_assert(vec1 + vec2 == vec1 * 2);
	static_assert(vec1 - vec2 == Vector2D::Zero());

	// Static methods
	auto vec3 = vec1;
	vec3 += vec2;
	vec3 /= 2U;
	assert(vec3 == vec2);

	vec3.Clear();
	assert(vec3 == Vector2D::Zero());

	// Methods
	array<int, 4> rgi1{};
	int rgi2[4]{};
	vec1.CopyToArray(rgi1);
	vec1.CopyToIter(rgi2);
	assert(
		rgi1[0] == rgi2[0] &&
		rgi1[1] == rgi2[1] &&
		rgi1[2] == rgi2[2] &&
		rgi1[3] == rgi2[3]
	);

	const auto gcemlen = gcem::sqrt(vec1.LengthSquared());
	const auto stdlen = std::sqrt(vec1.LengthSquared());

	//assert(vec1.Length() == std::sqrt(1 * 1 + 2 * 2));
	fmt::print(
		"============SQRT TESTS=============\n"
		"Vector2D::Length	{1:.{0}f} (Error: {2:.{0}f}%)\n"
		"gcem::sqrt		{3:.{0}f} (Error: {4:.{0}f}%)\n"
		"std::sqrt		{5:.{0}f}\n\n",
		std::numeric_limits<float>::max_digits10,
		vec1.Length(), (vec1.Length() - stdlen) / stdlen * 100.0f,
		gcemlen, (gcemlen - stdlen) / stdlen * 100.0f,
		stdlen
	);

	vec3 = vec1 / stdlen;

	fmt::print(
		"============NORM TESTS=============\n"
		"std::sqrt\n"
		"	X: {1:.{0}f}\n"
		"	Y: {2:.{0}f}\n"
		"Vector2D::Normalize\n"
		"	X: {3:.{0}f}\n"
		"	Y: {4:.{0}f}\n"
		"(Error: {5:.{0}f}%)\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec3.x, vec3.y,
		vec1.Normalize().x, vec1.Normalize().y,
		fmt::styled((vec3 - vec1.Normalize()).LengthSquared() * 100.0, fg(fmt::color::lime_green))
	);

	vec3 = vec1 * (2.0 / stdlen);

	fmt::print(
		"============SET LEN TESTS=============\n"
		"std::sqrt\n"
		"	X: {1:.{0}f}\n"
		"	Y: {2:.{0}f}\n"
		"Vector2D::SetLength\n"
		"	X: {3:.{0}f}\n"
		"	Y: {4:.{0}f}\n"
		"(Error: {5:.{0}f}%)\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec3.x, vec3.y,
		vec1.SetLength(2).x, vec1.SetLength(2).y,
		fmt::styled((vec3 - vec1.SetLength(2)).LengthSquared() / 2.0 * 100.0, fg(fmt::color::lime_green))
	);

	static_assert(!vec1.IsZero() && Vector2D::Zero().IsZero());
	static_assert(!vec2.IsNaN() && Vector2D(std::numeric_limits<float>::quiet_NaN(), 0).IsNaN());

	// Conversion
	//static_assert(!(bool)Vector2D::Zero() && (bool)vec1); #FIXME_UNKNOWN_BUG
	static_assert(Vector2D::I().Rotate(90) == Vector2D::J());
	assert((float)Vector2D::I() == (float)Vector2D::J() && gcem::round((real_t)vec3) == 2);

	// Linear Algebra
	static constexpr Vector2D vec4 = Vector2D::I().Rotate(45);
	static_assert(vec4.x == vec4.y);

	static_assert(Vector2D::I().Angle() == 0 && Vector2D::J().Angle() == 90);
	assert(vec4.Angle() == 45);

	// STL Containers Compatibility
	fmt::print(
		"============ITER TESTS=============\n"
		"begin/end(via for_each): {}\n"
		"rbegin/rend: {}\n\n",
		fmt::join(vec1, ", "),
		fmt::join(vec1.crbegin(), vec1.crend(), ", ")
	);

	static_assert(vec1.at(0) == 1 && vec2.at(1) == 2);
	//static_assert(vec1[1] == 2 && vec2[0] == 1);	#FIXME Cannot cast pointer type in compile-time.
	static_assert(*vec1.data() == 1);
	static_assert(vec2.front() == 1 && vec2.back() == 2);
	static_assert(!vec3.empty() && vec3.size() == vec3.max_size());

	Vector2D vec5(8, 9);
	vec5.fill(0);
	vec5.swap(vec3);
	assert(vec3 == Vector2D::Zero() && std::round(vec5.Length()) == 2);

	// External Helper Fn
	static_assert((Vector2D::I() ^ Vector2D::J()) == 90);
	static_assert(gcem::round(Vector2D::I().Rotate(60) ^ Vector2D::J().Rotate(-45)) == 15);
	static_assert(CrossProduct(vec1, Vector2D(3, 4)) == Vector(0, 0, -2));

	Log("Successful.\n");
}

void UnitTest_Vector(void) noexcept
{
	Log("Starting...");

	static_assert(std::is_standard_layout_v<Vector>);
	static_assert(std::ranges::range<Vector>);

	using std::array;
	static_assert(sizeof(Vector) == 3 * sizeof(vec_t));

	// Construction
	static constexpr Vector vecZero(0, 0, 0);
	static_assert(vecZero == Vector::Zero());

	static constexpr Vector vecUpgrade(Vector2D(7, 7), 777ULL);
	static_assert(vecUpgrade.x == 7 && vecUpgrade.y == 7 && vecUpgrade.z == 777.0);

	static constexpr Vector vec1({ 1, 2, 3 }), vec2({ 1, 2, 3, 4 });
	static_assert(vec1 == vec2 && vec1.x == 1 && vec2.y == 2 && vec1.z == vec2.z);

	static constexpr double rgdb1[] = { 1, 2, 3 };
	static constexpr unsigned int rgui1[] = { 1, 2, 3, 4 };
	static_assert(Vector(rgdb1) == Vector(rgui1) && Vector(rgdb1) == vec1 && Vector(rgui1) == vec2);

	static constexpr array<long double, Vector::max_size()> rgldb1{ 1, 2, 3 };
	static constexpr array<long long, Vector::size() + 1> rgll1{ 1, 2, 3, 4 };
	static_assert(Vector(rgldb1) == Vector(rgll1));

	// Operators
	static_assert(-vec1 == Vector{ -1, -2, -3 });
	static_assert(-vec1 != Vector{ -1.0f + std::numeric_limits<vec_t>::epsilon(), -2.0f, -3.0f });
	static_assert(vec1 < 4);
	static_assert(vec1 >= Vector::K());
	static_assert(vec1 + vec2 == vec1 * 2);
	static_assert(vec1 - vec2 == Vector::Zero());

	// Static methods
	auto vec3 = vec1;
	vec3 += vec2;
	vec3 /= 2U;
	assert(vec3 == vec2);

	vec3.Clear();
	assert(vec3 == Vector::Zero());

	static_assert(CrossProduct(Vector::I(), Vector::J()) == Vector::K());

	// Methods
	array<int, 4> rgi1{};
	int rgi2[4]{};
	vec1.CopyToArray(rgi1);
	vec1.CopyToIter(rgi2);
	assert(
		rgi1[0] == rgi2[0] &&
		rgi1[1] == rgi2[1] &&
		rgi1[2] == rgi2[2] &&
		rgi1[3] == rgi2[3]
	);

	const auto gcemlen = gcem::sqrt(vec1.LengthSquared());
	const auto stdlen = std::sqrt(vec1.LengthSquared());

	//assert(vec1.Length() == std::sqrt(1 * 1 + 2 * 2));
	fmt::print(
		"============SQRT TESTS=============\n"
		"Vector::Length	{1:.{0}f} (Error: {2:.{0}f}%)\n"
		"gcem::sqrt		{3:.{0}f} (Error: {4:.{0}f}%)\n"
		"std::sqrt		{5:.{0}f}\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec1.Length(), (vec1.Length() - stdlen) / stdlen * 100.0f,
		gcemlen, (gcemlen - stdlen) / stdlen * 100.0f,
		stdlen
	);

	vec3 = vec1 / stdlen;

	static_assert(vec1.Length2D() == Vector2D(1, 2).Length());
	static_assert(vec2.Length2DSquared() == Vector2D(1, 2).LengthSquared());

	fmt::print(
		"============NORM TESTS=============\n"
		"std::sqrt\n"
		"	X: {1:.{0}f}\n"
		"	Y: {2:.{0}f}\n"
		"	Z: {3:.{0}f}\n"
		"Vector::Normalize\n"
		"	X: {4:.{0}f}\n"
		"	Y: {5:.{0}f}\n"
		"	Z: {6:.{0}f}\n"
		"(Error: {7:.{0}f}%)\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec3.x, vec3.y, vec3.z,
		vec1.Normalize().x, vec1.Normalize().y, vec1.Normalize().z,
		fmt::styled((vec3 - vec1.Normalize()).LengthSquared() * 100.0, fg(fmt::color::lime_green))
	);

	vec3 = vec1 * (2.0 / stdlen);

	fmt::print(
		"============SET LEN TESTS=============\n"
		"std::sqrt\n"
		"	X: {1:.{0}f}\n"
		"	Y: {2:.{0}f}\n"
		"	Z: {3:.{0}f}\n"
		"Vector::SetLength\n"
		"	X: {4:.{0}f}\n"
		"	Y: {5:.{0}f}\n"
		"	Z: {6:.{0}f}\n"
		"(Error: {7:.{0}f}%)\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec3.x, vec3.y, vec3.z,
		vec1.SetLength(2).x, vec1.SetLength(2).y, vec1.SetLength(2).z,
		fmt::styled((vec3 - vec1.SetLength(2)).LengthSquared() / 2.0 * 100.0, fg(fmt::color::lime_green))
	);

	static_assert(!vec1.IsZero() && Vector2D::Zero().IsZero());
	static_assert(!vec2.IsNaN() && Vector2D(std::numeric_limits<float>::quiet_NaN(), 0).IsNaN());

	assert(vec3.Make2D().SetLength(2).Approx(Vector2D(1, 2).SetLength(2), 1e-5f));

	// Conversion
	//static_assert(!(bool)Vector2D::Zero() && (bool)vec1); #FIXME_UNKNOWN_BUG
	assert((float)Vector::I() == (float)Vector::J() && gcem::round((real_t)vec3) == 2);

	// Linear Algebra

	static constexpr Vector angle{ 48, -93, 19 };
	static_assert(DotProduct(angle.Forward(), angle.Right()) == 0);
	static_assert(DotProduct(angle.Right(), angle.Up()) == 0);
	static_assert(DotProduct(angle.Up(), angle.Forward()) < 1e-5f);
	static_assert(CrossProduct(angle.Forward(), angle.Right()) == -angle.Up());
	static_assert(CrossProduct(angle.Right(), angle.Up()) == -angle.Forward());
	static_assert(CrossProduct(angle.Up(), angle.Forward()) == -angle.Right());

	const auto [f, r, u] = angle.AngleVectors();
	assert(Vector::VectorsAngles(f, r, u) == angle);

	static_assert(Vector::Zero().Forward() == Vector::I());
	static_assert(Vector(0, 90, 0).Forward() == Vector::J());
	static_assert(Vector(-90, 0, 0).Forward() == Vector::K());

	static_assert(Vector::I().VectorAngles() == Vector::Zero());
	static_assert(Vector::J().VectorAngles() == Vector(0, 90, 0));
	static_assert(Vector::K().VectorAngles() == Vector(90, 0, 0));

	static_assert(Vector::I().RotateZ(90) == Vector::J());
	static_assert(Vector::J().RotateX(90) == Vector::K());
	static_assert(Vector::K().RotateY(90) == Vector::I());

	// STL Containers Compatibility
	fmt::print(
		"============ITER TESTS=============\n"
		"begin/end(via for_each): {}\n"
		"rbegin/rend: {}\n\n",
		fmt::join(vec1, ", "),
		fmt::join(vec1.crbegin(), vec1.crend(), ", ")
	);

	static_assert(vec1.at(0) == 1 && vec2.at(1) == 2 && vec1.at(2) == vec2.at(2));
	//static_assert(vec1[1] == 2 && vec2[0] == 1 && vec1[2] == vec2[2]);	#FIXME Cannot cast pointer type in compile-time.
	static_assert(*vec1.data() == 1);
	static_assert(vec2.front() == 1 && vec2.back() == 3);
	static_assert(!vec3.empty() && vec3.size() == vec3.max_size());

	Vector vec5(7, 8, 9);
	vec5.fill(0);
	vec5.swap(vec3);
	assert(vec3 == Vector::Zero() && (int)vec5.Length() == 2);

	// External Helper Fn
	static_assert((Vector::I() ^ Vector::J()) == (Vector::J() ^ Vector::K()));
	//static_assert(gcem::round(Vector2D::I().Rotate(60) ^ Vector2D::J().Rotate(-45)) == 15);

	static constexpr Vector vec6(1, 1, 1);
	static_assert((vec6 ^ Vector::I()) == (vec6 ^ Vector::J()) && (vec6 ^ Vector::J()) == (vec6 ^ Vector::K()));
	static_assert(DotProduct(vec6, Vector::I()) == DotProduct(vec6, Vector::J()));
	static_assert(DotProduct2D(vec6, Vector::K()) == 0);
	static_assert(
		CrossProduct(Vector::I(), Vector::J()) == Vector::K() &&
		CrossProduct(Vector::J(), Vector::K()) == Vector::I() &&
		CrossProduct(Vector::K(), Vector::I()) == Vector::J()
		);
	static_assert(CrossProduct(Vector::I(), vec6) == -CrossProduct(vec6, Vector::I()));

	Log("Successful.\n");
}
