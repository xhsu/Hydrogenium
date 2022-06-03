
#include <array>
#include <iostream>
#include <numbers>
#include <iomanip>

#include <cassert>
#include <cmath>

import UtlWinConsole;
import UtlLinearAlgebra;

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

void UnitTest_Vector2D(void) noexcept
{
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

	auto vec3 = vec1;
	vec3 += vec2;
	vec3 /= 2U;
	assert(vec3 == vec2);

	vec3.Clear();
	assert(vec3 == Vector2D::Zero());

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

	const float gcemlen = gcem::sqrt(vec1.LengthSquared());
	const float stdlen = std::sqrt(vec1.LengthSquared());

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

	//static_assert(!(bool)Vector2D::Zero() && (bool)vec1); #FIXME_UNKNOWN_BUG
	static_assert(Vector2D::I().Rotate(90) == Vector2D::J());

	constexpr Vector2D vec4 = Vector2D::I().Rotate(45);
	static_assert(vec4.x == vec4.y);

	static_assert(Vector2D::I().Angle() == 0 && Vector2D::J().Angle() == 90);
	assert(vec4.Angle() == 45);

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
	assert(vec3 == Vector2D::Zero() && (int)vec5.Length() == 2);
}

int main(int argc, char** args) noexcept
{
	UnitTest_Vector2D();
}
