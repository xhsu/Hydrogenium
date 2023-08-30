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

#include "UnitTest.hpp"

#ifndef _MSVC_LANG
typedef char __int8;
typedef short __int16;
typedef int __int32;
typedef long long __int64;
#endif

import UtlArithmetic;
import UtlConcepts;
//import UtlKeyValues;
import UtlLinearAlgebra;
import UtlRandom;
import UtlString;

using namespace std::string_literals;
using namespace std::string_view_literals;



extern void UnitTest_Vector2D(void) noexcept;
extern void UnitTest_Vector(void) noexcept;
extern void UnitTest_Angles(void) noexcept;

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

	static constexpr Matrix<3, 2> m3x2 =
	{
		{1, 2},
		{3, 4},
		{5, 6},
	};
	static constexpr Matrix<2, 3> m2x3 =
	{
		{10, 11, 12},
		{13, 14, 15},
	};
	static constexpr Matrix<m3x2.ROWS, m2x3.COLUMNS> m3x3 =
	{
		{36, 39, 42},
		{82, 89, 96},
		{128, 139, 150},
	};
	static constexpr Matrix<m2x3.ROWS, m3x2.COLUMNS> m2x2 =
	{
		{103, 136},
		{130, 172},
	};

	// Constructors
	static_assert(TransformMx() == TransformMx::Zero());

	static constexpr int rgrgi[3][3] =
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
	constexpr TransformMx mx1 =	// #FIXME due to MSVC error, or this should be 'static'
		TransformMx::Translate(7, 8)
		* TransformMx::Rotation(120)
		* TransformMx::Scale(4, 4, 1);

	static_assert(mx1 * Vector2::I() == Vector2(-0.5 * 4 + 7, std::numbers::sqrt3 / 2.0 * 4 + 8));

	// Properties && Operators (Inverse matrix would call all property functions)
	static_assert(-mx1 == -1.0 * mx1);

	static_assert((mx1 * ~mx1).Approx(TransformMx::Identity(), 1e-10));
	static_assert(~mx1 * (mx1 * Vector2(1, 2)) == Vector2(1, 2));

	static_assert(m3x2 * m2x3 == m3x3);
	static_assert(m2x3 * m3x2 == m2x2);

	static_assert(Matrix<1, 4>(1, 2, 3, 4) + Matrix<1, 4>(5, 6, 7, 8) == Matrix<1, 4>(6, 8, 10, 12));
	static_assert(Matrix<2, 2>(1, 2, 3, 4) - Matrix<2, 2>(5, 6, 7, 8) == Matrix<2, 2>(-4, -4, -4, -4));

	static_assert((Matrix<3, 1>(1, 0, 0) | Matrix<3, 1>(0, 1, 0) | Matrix<3, 1>(0, 0, 1)) == Matrix<3, 3>::Identity());

	// Methods
	static constexpr double DBL_NAN = std::numeric_limits<mxs_t>::quiet_NaN();
	static_assert(Matrix<2, 2>({ {DBL_NAN, 0}, {0, DBL_NAN} }).IsNaN());
	static_assert(!mx1.IsNaN());

	auto mx2 = TransformMx::Zero();
	mx2.ReplaceCol(2, 1, 2, 3);
	assert(mx2[0][2] == 1 && mx2[1][2] == 2 && mx2[2][2] == 3);
	mx2.ReplaceRow(2, 4, 5, 6);
	assert(mx2[2][0] == 4 && mx2[2][1] == 5 && mx2[2][2] == 6);

	static_assert(
		TransformMx::Identity().ToVector(0) == Vector3::I()
		&& TransformMx::Identity().ToVector(1) == Vector3::J()
		&& TransformMx::Identity().ToVector(2) == Vector3::K()
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
	static_assert([]<size_t... I>(std::index_sequence<I...> &&) consteval -> bool
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

	static constexpr auto v1 = Angles(45, 30, 60);
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
	static constexpr auto q3 = Quaternion(Vector3(1, 1, 1).Normalize(), 120);
	static constexpr auto q2 = Quaternion(45, 60, 90);
	static_assert(gcem::abs(q3.Norm() - q3.NormSquared()) < 1e-5 && gcem::abs(q3.Norm() - 1) < 1e-5);
	static_assert(q1.Conjugate() * q1 == Quaternion::Identity());
	static_assert((q1 * q2).Conjugate() == q2.Conjugate() * q1.Conjugate());	// conj(z1*z2) = conj(z2) * conj(z1)
	static_assert(q1 * q1.Conjugate() == Quaternion::Identity());
	static_assert(q4.Versor().Norm() - 1 < 1e-10);
	static_assert(q4.Reciprocal() * q4 == Quaternion::Identity());
	static_assert(q4 * ~q4 == Quaternion::Identity());
	static_assert(q4.Real() == 1 && q4.Pure() == Vector3(2, 3, 4));

	// Methods
	static_assert(Quaternion(std::numeric_limits<qtn_t>::quiet_NaN(), 2, 3, 4).IsNaN());
	static_assert(!q4.IsNaN());
	static_assert(!q4.IsZero());
	static_assert(Quaternion::Zero().IsZero());

	// Operators
	static_assert((q3 * 2 * 3 / 6).Norm() - 1 < 1e-10);
	static_assert(q3 * Vector3::I() == Vector3::J());
	static_assert(q3 * Vector3::J() == Vector3::K());
	static_assert(q3 * Vector3::K() == Vector3::I());
	static_assert(q3 * ~q3 == Quaternion::Identity());
	static_assert((~q2 * (q2 * Vector3(1, 2, 3))).Approx(Vector3(1, 2, 3), 1e-5f));

	// Conversion
	static_assert(q1.Euler() == v1);
	static_assert(q1.M3x3().Approx(Matrix<3, 3>::Rotation(30, 45, 60), 1e-10));
	static_assert(q3.M3x3().Approx(Matrix<3, 3>::Rotation(Vector3(1, 1, 1).Normalize(), 120), 1e-7));

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
	assert(std::find(pool.begin(), pool.end(), UTIL_GetRandomOne(pool)) != pool.end());
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
	using CharacterSet = VariadicTemplateWrapper<char, wchar_t, char16_t, char32_t>;
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
	static_assert(CharacterSet::Isomer_v<char16_t, char32_t, char, wchar_t>);
	static_assert(CharacterSet::Isomer_v<VariadicTemplateWrapper<char16_t, char32_t, char, wchar_t>>);
}

/*
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
	p->SetValue("Prime", std::views::iota(1) | std::views::filter(fnIsOdd) | std::views::filter(fnIsPrime) | std::views::take(24) | ::ranges::to<std::vector>);	// #FIXME_UNKNOWN_BUG Why I have to convert this into std::vector first??? std::views::common doesn't work.
	p->AccessEntry("Linear Algebra")->SetValue("Vector2", Vector2(std::numbers::inv_pi, std::numbers::pi));
	p->AccessEntry("Linear Algebra")->SetValue("Vector", Vector(std::numbers::sqrt2, std::numbers::sqrt3, gcem::sqrt(5.0)));
	p->AccessEntry("Linear Algebra")->SetValue("Quaternion", Quaternion(Vector(1, 1, 1).Normalize(), 120));
	p->SetValue("Escape", "This is a string containing '\"' // 'comment' with in same line would be included!\nA new line!!!");
	p->AccessEntry("UTF-8")->SetValue(u8"Latina", u8"Heraclius");
	p->AccessEntry("UTF-8")->SetValue(u8"Ελληνικά", u8"Ἡράκλειος");
	p->AccessEntry("UTF-8")->SetValue(u8"Français", u8"Héraclius");
	p->AccessEntry("UTF-8")->SetValue(u8"Русский", u8"Ираклий");
	p->AccessEntry("UTF-8")->SetValue(u8"日本語", u8"ヘラクレイオス");
	p->AccessEntry("UTF-8")->SetValue(u8"中文", u8"希拉克略");
	assert(p->SaveToFile("UnitTest_UtlKeyValues.txt"));
	assert(p->LoadFromFile("UnitTest_UtlKeyValues.txt"));

	assert((p->GetValue<long, float>("Prime") == std::pair{ 1L, 3.0f }));
	assert((p->GetValue<long, float, double>("Prime") == std::tuple{ 1L, 3.0f, 5.0 }));

	auto p2 = p->AccessEntry("Linear Algebra");
	assert(p2);

	assert(p2->GetValue<Vector2>("Vector2") == Vector2(std::numbers::inv_pi, std::numbers::pi));
	assert(p2->GetValue<Vector>("Vector") == Vector(std::numbers::sqrt2, std::numbers::sqrt3, gcem::sqrt(5.0)));
	assert(p2->GetValue<Quaternion>("Quaternion") == Quaternion(Vector(1, 1, 1).Normalize(), 120));

	assert((p->GetValue<std::array<unsigned, 3>>("Test") == std::array{ 1u, 2u, 3u }));
	assert((p->GetValue<std::array<std::string_view, 3>>("Test") == std::array{ "1"sv, "2"sv, "3"sv }));
	//assert((p->GetValue<std::vector<unsigned>>("Test") == std::vector{ 1u, 2u, 3u }));
	//assert((p->GetValue<std::vector<std::string>>("Test") == std::vector{ "1"s, "2"s, "3"s }));
	assert(p->GetValue<Vector2>("Test") == Vector2(1, 2));
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

	p->PrintC();

	delete p;
}*/

extern void UnitTest_UTF8Iterator() noexcept;
extern void UnitTest_ASCIIStringView(void) noexcept;
extern void UniTest_UTF8StringView(void) noexcept;

extern void UnitTest_Reflexpr(void) noexcept;


int main(int argc, char* argv[]) noexcept
{
	std::ios_base::sync_with_stdio(false);

	UnitTest_Vector2D();
	UnitTest_Vector();
	UnitTest_Angles();
	//UnitTest_Matrix();
	//UnitTest_Quaternion();
	//UnitTest_UtlArithmetic();
	//UnitTest_UtlRandom();
	//UnitTest_UtlConcepts();
	//UnitTest_UtlKeyValues();
	UnitTest_UTF8Iterator();
	UnitTest_ASCIIStringView();
	UniTest_UTF8StringView();
	UnitTest_Reflexpr();

	return EXIT_SUCCESS;
}
