#include <array>
#include <iomanip>
#include <iostream>
#include <numbers>
#include <ranges>
#include <source_location>
#include <vector>
#include <coroutine>
#include <experimental/generator>

#include <cassert>
#include <cmath>

#include "../gcem/include/gcem.hpp"
#include <fmt/color.h>
#include <fmt/ranges.h>
#include <range/v3/range.hpp>
#include <range/v3/view.hpp>

#ifndef _MSVC_LANG
typedef char __int8;
typedef short __int16;
typedef int __int32;
typedef long long __int64;
#endif

import UtlArithmetic;
import UtlConcepts;
import UtlKeyValues;
import UtlLinearAlgebra;
import UtlRandom;
import UtlString;

using namespace std::string_literals;
using namespace std::string_view_literals;

void Log(const auto& sz, std::source_location hSourceLocation = std::source_location::current()) noexcept
{
	//cout_gold() << std::format("[{}] {}:({}, {}): {}\n", hSourceLocation.file_name(), hSourceLocation.function_name(), hSourceLocation.line(), hSourceLocation.column(), sz) << white_text;
	fmt::print(
		fmt::emphasis::bold | fmt::emphasis::italic | fg(fmt::color::yellow),
		"[{}] {}:({}, {}): {}\n",
		hSourceLocation.file_name(), hSourceLocation.function_name(), hSourceLocation.line(), hSourceLocation.column(),
		sz
	);
}

// #DEV_TOOL unit test fn
//void UnitTest_ValveKeyValues(void) noexcept
//{
//	std::cout << std::format("Running unit test function of type \"{}\" in " __FILE__ "\n", typeid(ValveKeyValues).name());
//
//	constexpr double CONSTEXPR_C_ARRAY[3] = { gcem::sqrt(2), gcem::sqrt(3), gcem::sqrt(5) };
//
//	auto pkv = new ValveKeyValues(std::filesystem::path("SavingArrayText.txt"));
//
//	cout_pink() << pkv->GetValue<const char*>("EscapeTest") << '\n';
//
//	Vector2D v2 = pkv->GetValue<Vector2D>("Vector2D");
//	Vector v3 = pkv->GetValue<Vector>("Vector");
//	Quaternion q = pkv->GetValue<Quaternion>("Quaternion");
//	Color4b color4b = pkv->GetValue<Color4b>("Color4b");
//	Color4f color4f = pkv->GetValue<Color4f>("Color4f");
//
//	pkv->SetValue("v2", v2);
//	pkv->SetValue("v3", v3);
//	pkv->SetValue("q", q);
//	pkv->SetValue("color4b", color4b);
//	pkv->SetValue("color4f", color4f);
//
//	auto pkv2 = pkv->CreateEntry("Subsection");
//	pkv2->SetValue("init_list", 0b1, 0b11, 0b111, 0b1111, 0b11111);
//	pkv2->SetValue("array", std::array<float, 3>{1.414f, 1.732f, 2.236f});
//	pkv2->SetValue("c_array", CONSTEXPR_C_ARRAY);
//	pkv2->SetValue("vector", std::vector<double>{std::numbers::e, std::numbers::egamma, std::numbers::pi});
//	pkv2->SetValue("string", std::string { "what the hell" });
//	pkv2->SetValue("const char*", "WHAT THE HECK");
//	pkv2->SetValue("string_view", std::string_view { "Sectant" });
//	pkv2->SetValue("tuple", v2, v3, q, color4b, color4f);
//
//	pkv->SaveToFile("SavingArrayText_out.txt");
//
//	auto a1 = pkv2->GetValue<std::array<double, 2>>("init_list");
//	auto a2 = pkv2->GetValue<std::array<long double, 4>>("array");
//	auto v4 = pkv2->GetValue<std::vector<float>>("vector");
//
//	auto [_v1, _v2, _v3, _v4, _v5] = pkv2->GetValue<Color4b, Quaternion, Vector, Vector2D, Color4f>("tuple");
//}

void UnitTest_Vector(void) noexcept
{
	Log("Starting...");

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

	static constexpr array<long double, Vector::max_size()> rgldb1 { 1, 2, 3 };
	static constexpr array<long long, Vector::size() + 1> rgll1 { 1, 2, 3, 4 };
	static_assert(Vector(rgldb1) == Vector(rgll1));

	// Operators
	static_assert(-vec1 == Vector { -1, -2, -3 });
	static_assert(-vec1 != Vector { -1.0f + std::numeric_limits<vec_t>::epsilon(), -2.0f, -3.0f });
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
	array<int, 4> rgi1 {};
	int rgi2[4] {};
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
	std::cout << "============SQRT TESTS=============\n";
	std::cout << std::setprecision(std::numeric_limits<vec_t>::max_digits10 + 1)
		<< " - Vector::Length " << vec1.Length() << " (Error: " << (vec1.Length() - stdlen) / stdlen * 100.0f << "%)" << '\n'
		<< " - gcem::sqrt " << gcemlen << " (Error: " << (gcemlen - stdlen) / stdlen * 100.0f << "%)" << '\n'
		<< " - std::sqrt " << stdlen << "\n\n";

	vec3 = vec1 / stdlen;

	static_assert(vec1.Length2D() == Vector2D(1, 2).Length());
	static_assert(vec2.Length2DSquared() == Vector2D(1, 2).LengthSquared());

	std::cout << "============NORM TESTS=============\n";
	std::cout << " - std::sqrt \n" << vec3;
	std::cout << " - Vector::Normalize \n" << vec1.Normalize() << '\n';

	std::cout << "============SET LEN TESTS=============\n"
		<< " - std::sqrt\n" << (vec3 = vec1 * (2.0f / stdlen))
		<< " - Vector::SetLength\n" << vec1.SetLength(2) << '\n';

	static_assert(!vec1.IsZero() && Vector2D::Zero().IsZero());
	static_assert(!vec2.IsNaN() && Vector2D(std::numeric_limits<float>::quiet_NaN(), 0).IsNaN());

	assert(vec3.Make2D().SetLength(2).Approx(Vector2D(1, 2).SetLength(2), 1e-5f));

	// Conversion
	//static_assert(!(bool)Vector2D::Zero() && (bool)vec1); #FIXME_UNKNOWN_BUG
	assert((float)Vector::I() == (float)Vector::J() && gcem::round((real_t)vec3) == 2);

	// Linear Algebra

	constexpr Vector angle { 48, -93, 19 };
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
	std::cout << "============ITER TESTS=============\n";
	std::cout << "begin/end(via for_each): ";
	for (const auto& fl : vec1)
		std::cout << fl << ' ';
	std::cout << '\n';

	std::cout << "rbegin/rend: ";
	for (auto it = vec1.crbegin(); it != vec1.crend(); ++it)
		std::cout << *it << ' ';
	std::cout << '\n';

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

	constexpr Vector vec6(1, 1, 1);
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

void UnitTest_Vector2D(void) noexcept
{
	Log("Starting...");

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

	static constexpr array<long double, 2> rgldb1 { 1, 2 };
	static constexpr array<long long, 3> rgll1 { 1, 2, 3 };
	static_assert(Vector2D(rgldb1) == Vector2D(rgll1));

	// Operators
	static_assert(-vec1 == Vector2D { -1, -2 });
	static_assert(-vec1 != Vector2D { -1.0f + std::numeric_limits<float>::epsilon(), -2.0f });
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
	array<int, 4> rgi1 {};
	int rgi2[4] {};
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
	std::cout << "============SQRT TESTS=============\n";
	std::cout << std::setprecision(std::numeric_limits<float>::max_digits10 + 1)
		<< "Vector2D::Length " << vec1.Length() << " (Error: " << (vec1.Length() - stdlen) / stdlen * 100.0f << "%)" << '\n'
		<< "gcem::sqrt " << gcemlen << " (Error: " << (gcemlen - stdlen) / stdlen * 100.0f << "%)" << '\n'
		<< "std::sqrt " << stdlen << "\n\n";

	vec3 = vec1 / stdlen;

	std::cout << "============NORM TESTS=============\n";
	std::cout << "std::sqrt \n" << vec3;
	std::cout << "Vector2D::Normalize \n" << vec1.Normalize() << '\n';

	std::cout << "============SET LEN TESTS=============\n"
		<< "std::sqrt\n" << (vec3 = vec1 * (2.0f / stdlen))
		<< "Vector2D::SetLength\n" << vec1.SetLength(2) << '\n';

	static_assert(!vec1.IsZero() && Vector2D::Zero().IsZero());
	static_assert(!vec2.IsNaN() && Vector2D(std::numeric_limits<float>::quiet_NaN(), 0).IsNaN());

	// Conversion
	//static_assert(!(bool)Vector2D::Zero() && (bool)vec1); #FIXME_UNKNOWN_BUG
	static_assert(Vector2D::I().Rotate(90) == Vector2D::J());
	assert((float)Vector2D::I() == (float)Vector2D::J() && gcem::round((real_t)vec3) == 2);

	// Linear Algebra
	constexpr Vector2D vec4 = Vector2D::I().Rotate(45);
	static_assert(vec4.x == vec4.y);

	static_assert(Vector2D::I().Angle() == 0 && Vector2D::J().Angle() == 90);
	assert(vec4.Angle() == 45);

	// STL Containers Compatibility
	std::cout << "============ITER TESTS=============\n";
	std::cout << "begin/end(via for_each): ";
	for (const auto& fl : vec1)
		std::cout << fl << ' ';
	std::cout << '\n';

	std::cout << "rbegin/rend: ";
	for (auto it = vec1.crbegin(); it != vec1.crend(); ++it)
		std::cout << *it << ' ';
	std::cout << '\n';

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

void UnitTest_UtlArithmetic(void) noexcept
{
	Log("Starting...");

	static_assert(Hydrogenium::max(1, 2, 3, 4) == 4);

	int a = 1, b = 5, c = 9, d = 7;
	Hydrogenium::max(a, b, c, d) = 0;

	assert(c == 0);
	assert(Hydrogenium::max(a, b, c, d) == d);
	assert(&Hydrogenium::max(a, b, c, d) == &d);

	Log("Successful.\n");
}

void UnitTest_Matrix(void) noexcept
{
	Log("Starting...");

	using TransformMx = Matrix<3, 3>;
	using MxTestTy = Matrix<6, 4>;

	static_assert(std::ranges::range<TransformMx>);
	static_assert(std::ranges::range<MxTestTy>);

	static_assert(std::is_same_v<mxs_t(&)[MxTestTy::COLUMNS], MxTestTy::reference>);
	static_assert(std::is_same_v<mxs_t(*)[MxTestTy::COLUMNS], MxTestTy::pointer>);
	static_assert(std::is_same_v<mxs_t const(&)[MxTestTy::COLUMNS], MxTestTy::const_reference>);
	static_assert(std::is_same_v<mxs_t const(*)[MxTestTy::COLUMNS], MxTestTy::const_pointer>);
	static_assert(sizeof(TransformMx) == TransformMx::ROWS * TransformMx::COLUMNS * sizeof(mxs_t));
	static_assert(TransformMx::ROWS == 3 && TransformMx::COLUMNS == 3 && TransformMx::SQUARE_MX && TransformMx::RxC == 9 && TransformMx::DIAGONAL == 3);

	constexpr Matrix<3, 2> m3x2 =
	{
		{1, 2},
		{3, 4},
		{5, 6},
	};
	constexpr Matrix<2, 3> m2x3 =
	{
		{10, 11, 12},
		{13, 14, 15},
	};
	constexpr Matrix<m3x2.ROWS, m2x3.COLUMNS> m3x3 =
	{
		{36, 39, 42},
		{82, 89, 96},
		{128, 139, 150},
	};
	constexpr Matrix<m2x3.ROWS, m3x2.COLUMNS> m2x2 =
	{
		{103, 136},
		{130, 172},
	};

	// Constructors
	static_assert(TransformMx() == TransformMx::Zero());

	constexpr int rgrgi[3][3] =
	{
		{36, 39, 42},
		{82, 89, 96},
		{128, 139, 150},
	};
	static_assert(TransformMx(rgrgi) == m3x3);
	static_assert(TransformMx({ 36, 39, 42, 82, 89, 96, 128, 139, 150 }) == m3x3);
	static_assert(TransformMx(36, 39, 42, 82, 89, 96, 128, 139, 150) == m3x3);
	static_assert((Matrix<2, 2>)TransformMx(36, 39, 42, 82, 89, 96, 128, 139, 150) == Matrix<2, 2>(36, 39, 82, 89));

	// Static Methods
	constexpr TransformMx mx1 =
		TransformMx::Translate(7, 8)
		* TransformMx::Rotation(120)
		* TransformMx::Scale(4, 4, 1);

	static_assert(mx1 * Vector2D::I() == Vector2D(-0.5 * 4 + 7, std::numbers::sqrt3 / 2.0 * 4 + 8));

	// Properties && Operators (Inverse matrix would call all property functions)
	static_assert(-mx1 == -1.0 * mx1);

	static_assert((mx1 * ~mx1).Approx(TransformMx::Identity(), 1e-10));
	static_assert(~mx1 * (mx1 * Vector2D(1, 2)) == Vector2D(1, 2));

	static_assert(m3x2 * m2x3 == m3x3);
	static_assert(m2x3 * m3x2 == m2x2);

	static_assert(Matrix<1, 4>(1, 2, 3, 4) + Matrix<1, 4>(5, 6, 7, 8) == Matrix<1, 4>(6, 8, 10, 12));
	static_assert(Matrix<2, 2>(1, 2, 3, 4) - Matrix<2, 2>(5, 6, 7, 8) == Matrix<2, 2>(-4, -4, -4, -4));

	static_assert((Matrix<3, 1>(1, 0, 0) | Matrix<3, 1>(0, 1, 0) | Matrix<3, 1>(0, 0, 1)) == Matrix<3, 3>::Identity());

	// Methods
	constexpr double DBL_NAN = std::numeric_limits<mxs_t>::quiet_NaN();
	static_assert(Matrix<2, 2>({ {DBL_NAN, 0}, {0, DBL_NAN} }).IsNaN());
	static_assert(!mx1.IsNaN());

	auto mx2 = TransformMx::Zero();
	mx2.ReplaceCol(2, 1, 2, 3);
	assert(mx2[0][2] == 1 && mx2[1][2] == 2 && mx2[2][2] == 3);
	mx2.ReplaceRow(2, 4, 5, 6);
	assert(mx2[2][0] == 4 && mx2[2][1] == 5 && mx2[2][2] == 6);

	static_assert(
		TransformMx::Identity().ToVector(0) == Vector::I()
		&& TransformMx::Identity().ToVector(1) == Vector::J()
		&& TransformMx::Identity().ToVector(2) == Vector::K()
	);

	// Iterators
	std::cout << "============ITER TESTS=============\n";
	std::cout << "begin/end(via for_each):\n" << std::setprecision(4);
	for (const auto& row : mx1)
	{
		for (const auto& cell : row)
			std::cout << cell << '\t';
		std::cout << '\n';
	}

	std::cout << "rbegin/rend:\n";
	for (auto it = mx1.crbegin(); it != mx1.crend(); ++it)
	{
		for (const auto& cell : *it)
			std::cout << cell << '\t';
		std::cout << '\n';
	}

	// Element Access
	static_assert([&mx1]<size_t... I>(std::index_sequence<I...>&&) -> bool
	{
		return ((mx1[I / mx1.ROWS][I % mx1.COLUMNS] == mx1.at(I / mx1.ROWS)[I % mx1.COLUMNS]) && ...);
	}(std::make_index_sequence<mx1.RxC>{})
		);

	std::cout << "============'AT' ACCESSOR TESTS=============\n";
	bool bTryCatchTested = false;
	try
	{
		std::ignore = mx1.at(mx1.ROWS);
	}
	catch (const std::out_of_range& err)
	{
		bTryCatchTested = true;
		std::cout << "Capture: " << err.what() << '\n';
	}
	assert(bTryCatchTested);

	std::cout << "============DATA CONTINUOUS TESTS=============\n";
	for (auto p = mx1.data(), pend = mx1.data() + mx1.RxC; p != pend; ++p)
		std::cout << *p << '\t';
	std::cout << '\n';

	static_assert(std::equal(std::begin(mx1.front()), std::end(mx1.front()), std::begin(mx1[0]), std::end(mx1[0])));
	static_assert(std::equal(std::begin(mx1.back()), std::end(mx1.back()), std::begin(mx1[mx1.ROWS - 1]), std::end(mx1[mx1.ROWS - 1])));

	// Capacity
	static_assert(!mx1.empty() && mx1.size() == mx1.max_size() && mx1.size() == sizeof(mx1) / sizeof(mxs_t));

	// Modifiers
	mx2 = decltype(mx2)(rgrgi);
	mx2.fill(0);
	assert(mx2 == decltype(mx2)::Zero());

	auto mx3 = decltype(mx2)(rgrgi);
	mx2.swap(mx3);
	assert(mx2 == decltype(mx2)(rgrgi) && mx3 == decltype(mx3)::Zero());

	Log("Successful.\n");
}

void UnitTest_Quaternion(void) noexcept
{
	Log("Starting...");

	static_assert(std::ranges::range<Quaternion>);

	// Constructors
	static_assert(Quaternion{} == Quaternion::Identity());
	static_assert(Quaternion::Zero() == Quaternion(0, 0, 0, 0));
	static_assert(Quaternion(30, 45, 60) == Quaternion(Matrix<3, 3>::Rotation(30, 45, 60)));
	static_assert(Quaternion(30, 45, 60) != Quaternion(31, 46, 61));

	static constexpr auto v1 = Vector(45, 30, 60);
	static constexpr auto q1 = Quaternion(30, 45, 60);
	static_assert(v1.yaw == 30 && v1.pitch == 45 && v1.roll == 60);
	static_assert(q1 == Quaternion(v1));

	// Static Methods
	static_assert(
		Quaternion::I() * Quaternion::I() == -1 &&
		Quaternion::J() * Quaternion::J() == -1 &&
		Quaternion::K() * Quaternion::K() == -1 &&
		-1 == Quaternion::I() * Quaternion::J() * Quaternion::K()
		);	// Sir W. R. Hamilton's equation.

	// Properties
	static constexpr auto q4 = Quaternion(1, 2, 3, 4);
	static constexpr auto q3 = Quaternion(Vector(1, 1, 1).Normalize(), 120);
	static constexpr auto q2 = Quaternion(45, 60, 90);
	static_assert(gcem::abs(q3.Norm() - q3.NormSquared()) < 1e-5 && gcem::abs(q3.Norm() - 1) < 1e-5);
	static_assert(q1.Conjugate() * q1 == Quaternion::Identity());
	static_assert((q1 * q2).Conjugate() == q2.Conjugate() * q1.Conjugate());	// conj(z1*z2) = conj(z2) * conj(z1)
	static_assert(q1 * q1.Conjugate() == Quaternion::Identity());
	static_assert(q4.Versor().Norm() - 1 < 1e-10);
	static_assert(q4.Reciprocal() * q4 == Quaternion::Identity());
	static_assert(q4 * ~q4 == Quaternion::Identity());
	static_assert(q4.Real() == 1 && q4.Pure() == Vector(2, 3, 4));

	// Methods
	static_assert(Quaternion(std::numeric_limits<qtn_t>::quiet_NaN(), 2, 3, 4).IsNaN());
	static_assert(!q4.IsNaN());
	static_assert(!q4.IsZero());
	static_assert(Quaternion::Zero().IsZero());

	// Operators
	static_assert((q3 * 2 * 3 / 6).Norm() - 1 < 1e-10);
	static_assert(q3 * Vector::I() == Vector::J());
	static_assert(q3 * Vector::J() == Vector::K());
	static_assert(q3 * Vector::K() == Vector::I());
	static_assert(q3 * ~q3 == Quaternion::Identity());
	static_assert((~q2 * (q2 * Vector(1, 2, 3))).Approx(Vector(1, 2, 3), 1e-5f));

	// Conversion
	static_assert(q1.Euler() == v1);
	static_assert(q1.M3x3().Approx(Matrix<3, 3>::Rotation(30, 45, 60), 1e-10));
	static_assert(q3.M3x3().Approx(Matrix<3, 3>::Rotation(Vector(1, 1, 1).Normalize(), 120), 1e-7));

	// STL Containers Compatibility
	for (int i = 0; const auto & fl : q4)
		assert(++i == fl);

	{
		int i = 5;
		for (auto it = q4.crbegin(); it != q4.crend(); ++it)
			assert(--i == *it);
	}

	static_assert(q4.at(0) == 1 && q4.at(1) == 2 && q4.at(2) == 3 && q4.at(3) == 4);
	//static_assert(q4[0] == 1 && q4[1] == 2 && q4[2] == 3 && q4[3] == 4);	// #FIXME Cannot cast pointer type in compile-time.
	static_assert(*q4.data() == 1);
	static_assert(q4.front() == 1 && q4.back() == 4);
	static_assert(!q4.empty() && q4.size() == q4.max_size());

	Quaternion q5(5, 6, 7, 8), q6('f', 'u', 'c', 'k');
	q6.fill(0);
	q5.swap(q6);
	assert(q5 == Quaternion::Zero() && q6[2] == 7);

	Log("Successful.\n");
}

void UnitTest_UtlRandom(void) noexcept
{
	Log("Starting...");

	std::array pool{ 23, 192384, 1823947, 56738975, 2843598, 1039, 18345, 132948, 3984567, 354896 };
	assert(std::find(pool.begin(), pool.end(), *UTIL_GetRandomOne(pool)) != pool.end());
	assert(UTIL_Random(1u, 10u) < 11u && UTIL_Random(1, 10) > 0);
	assert(UTIL_Random(1.0f, 10.0f) < 10.1f && UTIL_Random(1.0, 10.0) > 0.999);

	std::array<unsigned, 10> counts{};
	std::array<double, 2> avg_err{ 0, 0 };
	constexpr auto TOTAL_RUN = 100000;

	std::cout << "============uniform_int_distribution TEST============\n";
	counts.fill(0);
	avg_err.fill(0);
	for (int i = 0; i < TOTAL_RUN; ++i)
		++counts[UTIL_Random(0, 9)];

	for (size_t i = 0; i < counts.size(); ++i)
	{
		const double rat = (double)counts[i] / (double)TOTAL_RUN;
		avg_err[rat < 0.1 ? 0 : 1] += (rat - 0.1) / 0.1;
		std::cout << " - " << i << ": " << counts[i] << " [" << std::setprecision(4) << (rat * 100) << "%]\n";
	}
	avg_err[0] /= 10;
	avg_err[1] /= 10;
	std::cout << std::format(" - Average error: [{:.4f}% - {:.4f}%]\n", avg_err[0] * 100, avg_err[1] * 100);

	std::cout << "============uniform_real_distribution TEST============\n";
	counts.fill(0);
	for (int i = 0; i < TOTAL_RUN; ++i)
		++counts[(size_t)std::round(UTIL_Random(-0.5, 9.5))];

	for (size_t i = 0; i < counts.size(); ++i)
	{
		const double rat = (double)counts[i] / (double)TOTAL_RUN;
		avg_err[rat < 0.1 ? 0 : 1] += (rat - 0.1) / 0.1;
		std::cout << " - " << i << ": " << counts[i] << " [" << std::setprecision(4) << (rat * 100) << "%]\n";
	}
	avg_err[0] /= 10;
	avg_err[1] /= 10;
	std::cout << std::format(" - Average error: [{:.4f}% - {:.4f}%]\n", avg_err[0] * 100, avg_err[1] * 100);

	std::cout << "============bernoulli distribution TEST============\n";
	counts.fill(0);
	avg_err.fill(0);
	for (int i = 0; i < TOTAL_RUN; ++i)
		++counts[UTIL_Random()];

	const double rat = (double)counts[1] / (double)counts[0];
	std::cout << " - false: " << counts[0] << '\n'
		<< " - true: " << counts[1] << '\n'
		<< " - t/f: " << std::setprecision(std::numeric_limits<double>::max_digits10 + 1) << rat << '\n'
		<< " - err: " << std::setprecision(2) << (rat - 1.0) * 100.0 << '%' << '\n';

	std::cout << "============SEEDED uniform_int_distribution TEST============\n";
	auto uiSeed = UTIL_Random(0U, 0xFFFFFFFF);
	counts.fill(0);
	avg_err.fill(0);

	for (int i = 0; i < TOTAL_RUN; ++i)
		++counts[UTIL_SeededRandom(uiSeed, 0, 9)];

	std::cout << " - Seed: " << uiSeed << '\n';
	for (size_t i = 0; i < counts.size(); ++i)
	{
		const double rat = (double)counts[i] / (double)TOTAL_RUN;
		avg_err[rat < 0.1 ? 0 : 1] += (rat - 0.1) / 0.1;
		std::cout << " - " << i << ": " << counts[i] << " [" << std::setprecision(4) << (rat * 100) << "%]\n";
	}
	avg_err[0] /= 10;
	avg_err[1] /= 10;
	std::cout << std::format(" - Average error: [{:.4f}% - {:.4f}%]\n", avg_err[0] * 100, avg_err[1] * 100);

	std::cout << "============SEEDED uniform_real_distribution TEST============\n";
	uiSeed = UTIL_Random(0U, 0xFFFFFFFF);
	counts.fill(0);
	avg_err.fill(0);

	for (int i = 0; i < TOTAL_RUN; ++i)
		++counts[(size_t)std::round(UTIL_SeededRandom(uiSeed, -0.5, 9.5))];

	std::cout << " - Seed: " << uiSeed << '\n';
	for (size_t i = 0; i < counts.size(); ++i)
	{
		const double rat = (double)counts[i] / (double)TOTAL_RUN;
		avg_err[rat < 0.1 ? 0 : 1] += (rat - 0.1) / 0.1;
		std::cout << " - " << i << ": " << counts[i] << " [" << std::setprecision(4) << (rat * 100) << "%]\n";
	}
	avg_err[0] /= 10;
	avg_err[1] /= 10;
	std::cout << std::format(" - Average error: [{:.4f}% - {:.4f}%]\n", avg_err[0] * 100, avg_err[1] * 100);

	Log("Successful.\n");
}

void UnitTest_UtlConcepts(void) noexcept
{
	static_assert(AnySame<char, __int8, __int16, __int32, __int64>);
	static_assert(!AnySame<float, __int8, __int16, __int32, __int64>);

	static_assert(AnyOrder<std::tuple<int, float, double>, float, double, int>);
	static_assert(!AnyOrder<std::tuple<char, float, double>, float, double, bool>);
	static_assert(!AnyOrder<std::tuple<int, float>, float, double, int>);
	static_assert(!AnyOrder<std::tuple<int, float, double>, float, double>);

	using IntegralSet = VariadicTemplateWrapper<__int8, __int16, __int32, __int64>;
	using FloatingPointSet = VariadicTemplateWrapper<float, double, long double>;
	using CharacterSet = VariadicTemplateWrapper<char, char8_t, wchar_t, char16_t, char32_t>;
	using FourLongs = VariadicTemplateWrapper<long, long, long, long>;
	static_assert(std::same_as<IntegralSet::type<0>, __int8>);
	static_assert(std::same_as<IntegralSet::type<3>, __int64>);
	static_assert(!IntegralSet::AllSame_v);
	static_assert(FourLongs::AllSame_v);
	static_assert(IntegralSet::Count_v == FourLongs::Count_v);
	static_assert(CharacterSet::Exists_v<char>);
	static_assert(!IntegralSet::Exists_v<long>);
	static_assert(std::same_as<IntegralSet::Tuple_t, std::tuple<char, short, int, long long>>);
	static_assert(IntegralSet::value<long> == 0);
	static_assert(FloatingPointSet::value<double> == 1);
	static_assert(FourLongs::value<long> == 4);
	static_assert(IntegralSet::npos == std::numeric_limits<std::size_t>::max());
	static_assert(FloatingPointSet::Index_v<long double> == 2);
	static_assert(FourLongs::Index_v<__int32> == FourLongs::npos);
	static_assert(std::same_as<IntegralSet::type<IntegralSet::Index_v<__int16>>, __int16>);
	static_assert(CharacterSet::Isomer_v<char16_t, char32_t, char, wchar_t, char8_t>);
	//static_assert(CharacterSet::Isomer_v<VariadicTemplateWrapper<char16_t, char32_t, char, wchar_t, char8_t>>);
}

void UnitTest_UtlKeyValues(void) noexcept
{
	static constexpr auto fnIsOdd = [](int i) constexpr { return i % 2; };
	static constexpr auto fnIsPrime = [](int i) constexpr
	{
		for (int j = 2; j * j <= i; ++j)
			if (i % j == 0)
				return false;

		return true;
	};

	auto p = new ValveKeyValues("UnitTest_UtlKeyValues");
	p->SetValue("Test", std::array{ 1, 2, 3 }, '4', "5", std::make_tuple(6.0, 7.0f, 8L), "9"sv, std::make_pair(10LL, 11ULL), (long double)12);
	p->SetValue("Prime", std::views::iota(1) | std::views::filter(fnIsOdd) | std::views::filter(fnIsPrime) | std::views::take(24) | ::ranges::to<std::vector>);	// #FIXME_UNKNOWN_BUG Why I have to convert this into std::vector first???
	p->CreateEntry("Linear Algebra")->SetValue("Vector2D", Vector2D(std::numbers::inv_pi, std::numbers::pi));
	p->FindEntry("Linear Algebra")->SetValue("Vector", Vector(std::numbers::sqrt2, std::numbers::sqrt3, gcem::sqrt(5.0)));
	p->FindEntry("Linear Algebra")->SetValue("Quaternion", Quaternion(Vector(1, 1, 1).Normalize(), 120));
	assert(p->SaveToFile("UnitTest_UtlKeyValues.txt"));
	assert(p->LoadFromFile("UnitTest_UtlKeyValues.txt"));

	assert((p->GetValue<long, float>("Prime") == std::pair{ 1L, 3.0f }));
	assert((p->GetValue<long, float, double>("Prime") == std::tuple{ 1L, 3.0f, 5.0 }));

	auto p2 = p->FindEntry("Linear Algebra");
	assert(p2);

	assert(p2->GetValue<Vector2D>("Vector2D") == Vector2D(std::numbers::inv_pi, std::numbers::pi));
	assert(p2->GetValue<Vector>("Vector") == Vector(std::numbers::sqrt2, std::numbers::sqrt3, gcem::sqrt(5.0)));
	assert(p2->GetValue<Quaternion>("Quaternion") == Quaternion(Vector(1, 1, 1).Normalize(), 120));

	assert((p->GetValue<std::array<unsigned, 3>>("Test") == std::array{ 1u, 2u, 3u }));
	assert((p->GetValue<std::array<std::string_view, 3>>("Test") == std::array{ "1"sv, "2"sv, "3"sv }));
	//assert((p->GetValue<std::vector<unsigned>>("Test") == std::vector{ 1u, 2u, 3u }));
	//assert((p->GetValue<std::vector<std::string>>("Test") == std::vector{ "1"s, "2"s, "3"s }));
	assert(p->GetValue<Vector2D>("Test") == Vector2D(1, 2));
	assert(p->GetValue<Vector>("Test") == Vector(1, 2, 3));
	assert(p->GetValue<Quaternion>("Test") == Quaternion(1, 2, 3, 4));
	assert((p->GetValue<double, int>("Test") == std::make_pair(1.0, 2)));
	assert((p->GetValue<double, int, long>("Test") == std::make_tuple(1.0, 2, 3L)));

	static_assert(requires(ValveKeyValues vkv) { { vkv.GetValue<double>("") } -> std::same_as<double>; });
	static_assert(requires(ValveKeyValues vkv) { { vkv.GetValue<double, int>("") } -> std::same_as<std::pair<double, int>>; });
	static_assert(requires(ValveKeyValues vkv) { { vkv.GetValue<double, int, long>("") } -> std::same_as<std::tuple<double, int, long>>; });

	auto const [vec1, str2, longlong3, short4, char5, float6, double7, rgstr8, rgstr9]
		= p->GetValue<Vector, std::string, long long, short, char, float, double, std::string[2], std::array<std::string, 3>>("Test");

	assert(vec1 == Vector(1, 2, 3));
	assert(str2 == "4"s);
	assert(longlong3 == 5LL);
	assert(short4 == (short)6);
	assert(char5 == '\x7');
	assert(float6 == 8.0f);
	assert(double7 == 9.0);
	assert(rgstr8[0] == "10"s);
	assert(rgstr8[1] == "11"s);
	assert((rgstr9 == std::array{ "12"s, ""s, ""s }));
}


int main(int argc, char* args[]) noexcept
{
	std::ios_base::sync_with_stdio(false);

	//UnitTest_Vector2D();
	//UnitTest_Vector();
	//UnitTest_Matrix();
	//UnitTest_Quaternion();
	//UnitTest_UtlArithmetic();
	//UnitTest_UtlRandom();
	//UnitTest_UtlConcepts();
	UnitTest_UtlKeyValues();

	return EXIT_SUCCESS;
}
