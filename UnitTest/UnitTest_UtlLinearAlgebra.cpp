//#include <array>

#include <cassert>

#include <fmt/color.h>
#include <fmt/ranges.h>

//#include "..\gcem\include\gcem.hpp"

#include "UnitTest.hpp"

import UtlLinearAlgebra;

void UnitTest_Vector2D(void) noexcept
{
	Log("Starting...");

	//static_assert(std::is_trivial_v<Vector2>);
	static_assert(std::is_standard_layout_v<Vector2>);
	static_assert(std::ranges::range<Vector2>);

	using std::array;
	static_assert(sizeof(Vector2) == 2 * sizeof(vec_t));

	// Construction
	static constexpr Vector2 vecZero(0, 0);
	static_assert(vecZero == Vector2::Zero());

	//static constexpr Vector2 vecQuad(7);
	//static_assert(vecQuad.width == 7 && vecQuad.height == 7);

	static constexpr Vector2 vec1(array{ 1, 2 }), vec2(array{ 1, 2, 3 });
	static_assert(vec1 == vec2 && vec1.x == 1 && vec2.y == 2);

	static constexpr double rgdb1[] = { 1, 2 };
	static constexpr unsigned int rgui1[] = { 1, 2, 3 };
	static_assert(Vector2(rgdb1) == Vector2(rgui1) && Vector2(rgdb1) == vec1 && Vector2(rgui1) == vec2);

	static constexpr array<long double, 2> rgldb1{ 1, 2 };
	static constexpr array<long long, 3> rgll1{ 1, 2, 3 };
	static_assert(Vector2(rgldb1) == Vector2(rgll1));

	// Operators
	static_assert(-vec1 == Vector2{ -1, -2 });
	static_assert(-vec1 != Vector2{ -1.0f + std::numeric_limits<float>::epsilon(), -2.0f });
	static_assert(vec1 < 3);
	static_assert(vec1 >= Vector2::J());
	static_assert(vec1 + vec2 == vec1 * 2);
	static_assert(vec1 - vec2 == Vector2::Zero());

	// Static methods
	auto vec3 = vec1;
	vec3 += vec2;
	vec3 /= 2U;
	assert(vec3 == vec2);

	vec3.Clear();
	assert(vec3 == Vector2::Zero());

	// Methods
	array<int, 4> rgi1{};
	int rgi2[4]{};
	vec1.CopyToRange(rgi1);	// CopyToArray
	vec1.CopyToRange(rgi2);	// CopyToIter
	assert(
		rgi1[0] == rgi2[0] &&
		rgi1[1] == rgi2[1] &&
		rgi1[2] == rgi2[2] &&
		rgi1[3] == rgi2[3]
	);

	const auto gcemlen = gcem::sqrt(vec1.LengthSquared());
	const auto stdlen = std::sqrt(vec1.LengthSquared());

	fmt::print(
		"============SQRT TESTS=============\n"
		"Vector2::Length	{1:.{0}f} (Error: {2:.{0}f}%)\n"
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
		"Vector2::Normalize\n"
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
		"Vector2::SetLength\n"
		"	X: {3:.{0}f}\n"
		"	Y: {4:.{0}f}\n"
		"(Error: {5:.{0}f}%)\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec3.x, vec3.y,
		vec1.SetLength(2).x, vec1.SetLength(2).y,
		fmt::styled((vec3 - vec1.SetLength(2)).LengthSquared() / 2.0 * 100.0, fg(fmt::color::lime_green))
	);

	static_assert(!vec1.IsZero() && Vector2::Zero().IsZero());
	static_assert(!vec2.IsNaN() && Vector2(std::numeric_limits<float>::quiet_NaN(), 0).IsNaN());

	// Conversion
	static_assert(!(bool)Vector2::Zero() && (bool)vec1); // #FIXME_UNKNOWN_BUG
	static_assert(Vector2::I().Rotate(90) == Vector2::J());
	assert((float)Vector2::I() == (float)Vector2::J() && gcem::round((real_t)vec3) == 2);

	// Linear Algebra
	static constexpr Vector2 vec4 = Vector2::I().Rotate(45);
	static_assert(vec4.x == vec4.y);

	static_assert(Vector2::I().Angle() < 1e-5 && Vector2::J().Angle() - 90 < 1e-5);
	static_assert(vec4.Angle() - 45 < 1e-10);

	// STL Containers Compatibility
	fmt::print(
		"============ITER TESTS=============\n"
		"begin/end(via for_each): {}\n"
		"rbegin/rend: {}\n\n",
		fmt::join(vec1, ", "),
		fmt::join(vec1.crbegin(), vec1.crend(), ", ")
	);

	static_assert(vec1.at(0) == 1 && vec2.at(1) == 2);
	static_assert(vec1[1] == 2 && vec2[0] == 1);	// #FIXME Cannot cast pointer type in compile-time.
	static_assert(*vec1.data() == 1);
	static_assert(vec2.front() == 1 && vec2.back() == 2);
	static_assert(!vec3.empty() && vec3.size() == vec3.max_size());

	Vector2 vec5(8, 9);
	vec5.fill(0);
	vec5.swap(vec3);
	assert(vec3 == Vector2::Zero() && std::round(vec5.Length()) == 2);

	// External Helper Fn
	static_assert((Vector2::I() ^ Vector2::J()) == 90);
	static_assert(gcem::round(Vector2::I().Rotate(60) ^ Vector2::J().Rotate(-45)) == 15);
	static_assert(CrossProduct(vec1, Vector2(3, 4)) == Vector3(0, 0, -2));

	Log("Successful.\n");
}

void UnitTest_Vector(void) noexcept
{
	Log("Starting...");

	static_assert(std::is_standard_layout_v<Vector3>);
	static_assert(std::ranges::range<Vector3>);

	using std::array;
	static_assert(sizeof(Vector3) == 3 * sizeof(vec_t));

	// Construction
	static constexpr Vector3 vecZero(0, 0, 0);
	static_assert(vecZero == Vector3::Zero());

	static constexpr Vector3 vecUpgrade(Vector2(7, 7), 777ULL);
	static_assert(vecUpgrade.x == 7 && vecUpgrade.y == 7 && vecUpgrade.z == 777.0);

	static constexpr double rgdb1[] = { 1, 2, 3 };
	static constexpr unsigned int rgui1[] = { 1, 2, 3, 4 };
	static constexpr Vector3 vec1(rgdb1), vec2(rgui1);
	static_assert(vec1 == vec2 && vec1.x == 1 && vec2.y == 2 && vec1.z == vec2.z);

	static constexpr array<long double, vec1.max_size()> rgldb1{ 1, 2, 3 };
	static constexpr array<long long, vec1.size() + 1> rgll1{ 1, 2, 3, 4 };
	static_assert(Vector3(rgldb1) == Vector3(rgll1));

	// Operators
	static_assert(-vec1 == Vector3{ -1, -2, -3 });
	static_assert(-vec1 != Vector3{ -1.0f + std::numeric_limits<vec_t>::epsilon(), -2.0f, -3.0f });
	static_assert(vec1 < 4);
	static_assert(vec1 >= Vector3::K());
	static_assert(vec1 + vec2 == vec1 * 2);
	static_assert(vec1 - vec2 == Vector3::Zero());

	// Static methods
	auto vec3 = vec1;
	vec3 += vec2;
	vec3 /= 2U;
	assert(vec3 == vec2);

	vec3.Clear();
	assert(vec3 == Vector3::Zero());

	static_assert(CrossProduct(Vector3::I(), Vector3::J()) == Vector3::K());

	// Methods
	array<int, 4> rgi1{};
	int rgi2[4]{};
	vec1.CopyToRange(rgi1);	// CopyToArray
	vec1.CopyToRange(rgi2);	// CopyToIter
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
		"Vector3::Length	{1:.{0}f} (Error: {2:.{0}f}%)\n"
		"gcem::sqrt		{3:.{0}f} (Error: {4:.{0}f}%)\n"
		"std::sqrt		{5:.{0}f}\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec1.Length(), (vec1.Length() - stdlen) / stdlen * 100.0f,
		gcemlen, (gcemlen - stdlen) / stdlen * 100.0f,
		stdlen
	);

	vec3 = vec1 / stdlen;

	static_assert(vec1.Length<2>() == Vector2(1, 2).Length());
	static_assert(vec2.LengthSquared<2>() == Vector2(1, 2).LengthSquared());

	fmt::print(
		"============NORM TESTS=============\n"
		"std::sqrt\n"
		"	X: {1:.{0}f}\n"
		"	Y: {2:.{0}f}\n"
		"	Z: {3:.{0}f}\n"
		"Vector3::Normalize\n"
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
		"Vector3::SetLength\n"
		"	X: {4:.{0}f}\n"
		"	Y: {5:.{0}f}\n"
		"	Z: {6:.{0}f}\n"
		"(Error: {7:.{0}f}%)\n\n",
		std::numeric_limits<vec_t>::max_digits10,
		vec3.x, vec3.y, vec3.z,
		vec1.SetLength(2).x, vec1.SetLength(2).y, vec1.SetLength(2).z,
		fmt::styled((vec3 - vec1.SetLength(2)).LengthSquared() / 2.0 * 100.0, fg(fmt::color::lime_green))
	);

	static_assert(!vec1.IsZero() && Vector2::Zero().IsZero());
	static_assert(!vec2.IsNaN() && Vector2(std::numeric_limits<float>::quiet_NaN(), 0).IsNaN());

	assert(vec3.Cast<2>().SetLength(2).Approx(Vector2(1, 2).SetLength(2), 1e-5f));

	// Conversion
	//static_assert(!(bool)Vector2::Zero() && (bool)vec1); #FIXME_UNKNOWN_BUG
	assert((float)Vector3::I() == (float)Vector3::J() && gcem::round((real_t)vec3) == 2);

	// Linear Algebra

	static_assert(Vector3::I().RotateZ(90) == Vector3::J());
	static_assert(Vector3::J().RotateX(90) == Vector3::K());
	static_assert(Vector3::K().RotateY(90) == Vector3::I());

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

	Vector3 vec5(7, 8, 9);
	vec5.fill(0);
	vec5.swap(vec3);
	assert(vec3 == Vector3::Zero() && (int)vec5.Length() == 2);

	// External Helper Fn
	static_assert((Vector3::I() ^ Vector3::J()) == (Vector3::J() ^ Vector3::K()));
	static_assert(gcem::round(Vector2::I().Rotate(60) ^ Vector2::J().Rotate(-45)) == 15);

	static constexpr Vector3 vec6(1, 1, 1);
	static_assert((vec6 ^ Vector3::I()) == (vec6 ^ Vector3::J()) && (vec6 ^ Vector3::J()) == (vec6 ^ Vector3::K()));
	static_assert(DotProduct(vec6, Vector3::I()) == DotProduct(vec6, Vector3::J()));
	static_assert(DotProduct(vec6.Cast<2>(), Vector3::K().Cast<2>()) == 0);
	static_assert(
		CrossProduct(Vector3::I(), Vector3::J()) == Vector3::K() &&
		CrossProduct(Vector3::J(), Vector3::K()) == Vector3::I() &&
		CrossProduct(Vector3::K(), Vector3::I()) == Vector3::J()
		);
	static_assert(CrossProduct(Vector3::I(), vec6) == -CrossProduct(vec6, Vector3::I()));

	Log("Successful.\n");
}

void UnitTest_Angles(void) noexcept
{
	Log("Starting...");

	static constexpr Angles angle{ 48, -93, 19 };
	static_assert(DotProduct(angle.Front(), angle.Right()) == 0);
	static_assert(DotProduct(angle.Right(), angle.Up()) == 0);
	static_assert(DotProduct(angle.Up(), angle.Front()) < 1e-5f);
	static_assert(CrossProduct(angle.Front(), angle.Right()).Approx(-angle.Up(), 1e-5f));
	static_assert(CrossProduct(angle.Right(), angle.Up()).Approx(-angle.Front(), 1e-5f));
	static_assert(CrossProduct(angle.Up(), angle.Front()).Approx(-angle.Right(), 1e-5f));

	static constexpr auto vecs = angle.AngleVectors();	// #REPORT_TO_MSVC_evaluate_const_in_static_assert
	static_assert(Angles::VectorsAngles(std::get<0>(vecs), std::get<1>(vecs), std::get<2>(vecs)).Approx(angle, 1e-5f));

	static_assert(Angles::Forward().Front().Approx(Vector3::I(), 1e-5f));
	static_assert(Angles::Leftward().Front().Approx(Vector3::J(), 1e-5f));
	static_assert(Angles::Downwards().Front().Approx(Vector3::K(), 1e-5f));

	static_assert(Vector3::I().VectorAngles().Approx(Angles::Forward(), 1e-5f));
	static_assert(Vector3::J().VectorAngles().Approx(Angles::Leftward(), 1e-5f));
	//static_assert(Vector3::K().VectorAngles().Approx(Angles::Downwards(), 1e-5f));

	Log("Successful.\n");
}
