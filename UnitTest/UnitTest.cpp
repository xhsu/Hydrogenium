
#include <array>
#include <iostream>
#include <numbers>
#include <iomanip>
#include <source_location>
#include <format>

#include <cassert>
#include <cmath>

#include "../gcem/include/gcem.hpp"

import UtlWinConsole;
import UtlLinearAlgebra;
import UtlConcepts;
import UtlArithmetic;

template <typename T>
void Log(const T& sz, std::source_location hSourceLocation = std::source_location::current()) noexcept
{
	cout_gold() << std::format("[{}] {}:({}, {}): {}\n", hSourceLocation.file_name(), hSourceLocation.function_name(), hSourceLocation.line(), hSourceLocation.column(), sz);
	cout_w();
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

	using std::array;

	// Construction
	constexpr Vector vecZero(0, 0, 0);
	static_assert(vecZero == Vector::Zero());

	constexpr Vector vecUpgrade(Vector2D(7, 7), 777ULL);
	static_assert(vecUpgrade.x == 7 && vecUpgrade.y == 7 && vecUpgrade.z == 777.0);

	constexpr Vector vec1({ 1, 2, 3 }), vec2({ 1, 2, 3, 4 });
	static_assert(vec1 == vec2 && vec1.x == 1 && vec2.y == 2 && vec1.z == vec2.z);

	constexpr double rgdb1[] = { 1, 2, 3 };
	constexpr unsigned int rgui1[] = { 1, 2, 3, 4 };
	static_assert(Vector(rgdb1) == Vector(rgui1) && Vector(rgdb1) == vec1 && Vector(rgui1) == vec2);

	constexpr array<long double, Vector::max_size()> rgldb1 { 1, 2, 3 };
	constexpr array<long long, Vector::size() + 1> rgll1 { 1, 2, 3, 4 };
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

	const Vector angle { 48, -93, 19 };
	assert(DotProduct(angle.Forward(), angle.Right()) == 0);
	assert(DotProduct(angle.Right(), angle.Up()) == 0);
	assert(DotProduct(angle.Up(), angle.Forward()) < 1e-5f);
	assert(CrossProduct(angle.Forward(), angle.Right()) == -angle.Up());
	assert(CrossProduct(angle.Right(), angle.Up()) == -angle.Forward());
	assert(CrossProduct(angle.Up(), angle.Forward()) == -angle.Right());

	const auto [f, r, u] = angle.AngleVectors();
	assert(Vector::VectorsAngles(f, r, u) == angle);

	assert(Vector::Zero().Forward() == Vector::I());
	assert(Vector(0, 90, 0).Forward() == Vector::J());
	assert(Vector(-90, 0, 0).Forward() == Vector::K());

	assert(Vector::I().VectorAngles() == Vector::Zero());
	assert(Vector::J().VectorAngles() == Vector(0, 90, 0));
	assert(Vector::K().VectorAngles() == Vector(90, 0, 0));

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

	using std::array;

	// Construction
	constexpr Vector2D vecZero(0, 0);
	static_assert(vecZero == Vector2D::Zero());

	constexpr Vector2D vecQuad(7);
	static_assert(vecQuad.width == 7 && vecQuad.height == 7);

	constexpr Vector2D vec1({ 1, 2 }), vec2({ 1, 2, 3 });
	static_assert(vec1 == vec2 && vec1.x == 1 && vec2.y == 2);

	constexpr double rgdb1[] = { 1, 2 };
	constexpr unsigned int rgui1[] = { 1, 2, 3 };
	static_assert(Vector2D(rgdb1) == Vector2D(rgui1) && Vector2D(rgdb1) == vec1 && Vector2D(rgui1) == vec2);

	constexpr array<long double, 2> rgldb1 { 1, 2 };
	constexpr array<long long, 3> rgll1 { 1, 2, 3 };
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

	assert(Hydrogenium::max(1, 2, 3, 4) == 4);

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

	constexpr TransformMx mx =
		TransformMx::Translate(7, 8)
		* TransformMx::Rotation(120)
		* TransformMx::Scale(4, 4, 1);

	static_assert(mx * Vector2D::I() == Vector2D(-0.5*4+7, std::numbers::sqrt3 / 2.0*4+8));

	std::cout << mx * Vector2D::I() << '\n';

	Log("Successful.\n");
}

int main(int argc, char** args) noexcept
{
	UnitTest_Vector2D();
	UnitTest_Vector();
	UnitTest_UtlArithmetic();
	UnitTest_Matrix();
}
