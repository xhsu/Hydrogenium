#include <cassert>

#include <fmt/color.h>

import <cstdint>;

import <array>;
import <bit>;
import <concepts>;
import <numbers>;

import UtlHook;

using std::array;
using std::bit_cast;

struct HookVictim_t
{
	// Virtual table tests
	virtual void FUNC1(void) const noexcept { fmt::print("I am original func1!\n"); }
	virtual int FUNC2(int arg1, float arg2) const noexcept { fmt::print("I am original func2! [int: {}, float: {}]\n", arg1, arg2); return 0xDEAD; }
	virtual bool FUNC3(bool arg1) const noexcept { fmt::print("I am original func3! [member == {}]\n", arg1 ? m_1 : m_2); return arg1; }

	// Member function tests
	unsigned char FUNC4(array<double, 3> const &vec) noexcept { fmt::print("I am original func4! [vec == {}]\n", fmt::join(vec, ", ")); m_vec = vec; return 0x03; }

	// Global function test
	static array<double, 3> FUNC5(double arg1, double arg2, double arg3) noexcept { fmt::print("I am original func5! [{}, {}, {}]\n", arg1, arg2, arg3); return { arg1, arg2, arg3 }; }

	// Member variable tests
	double m_1 = std::numbers::pi;
	double m_2 = std::numbers::e;
	array<double, 3> m_vec{ 0, 0, 0 };
};

void _Hook1(HookVictim_t const *pThis) noexcept { fmt::print("I am replaced func1!\n"); }
int _Hook2(HookVictim_t const *pThis, int arg1, float arg2) noexcept { fmt::print("I am replaced func2! [int: {}, float: {}]\n", arg1, arg2); return 0xBEEF; }
bool _Hook3(HookVictim_t const *pThis, bool arg1) noexcept { fmt::print("I am replaced func3! [member == {}]\n", arg1 ? pThis->m_1 : pThis->m_2); return !arg1; }
unsigned char _Hook4(HookVictim_t *pThis, array<double, 3> const &vec) noexcept { pThis->m_vec = { vec[0] - 1, vec[1] - 1, vec[2] - 1 }; fmt::print("I am replaced func4! [vec == {}]\n", fmt::join(pThis->m_vec, ", ")); return 0x12; }
array<double, 3> _Hook5(double arg1, double arg2, double arg3) noexcept { fmt::print("I am replaced func5! [{}, {}, {}]\n", arg1, arg2, arg3); return { arg1 / 2, arg2 / 2, arg3 / 2 }; }

using FUNC1_TY = void(__thiscall *)(HookVictim_t const *) noexcept;
using FUNC2_TY = int(__thiscall *)(HookVictim_t const *, int, float) noexcept;
using FUNC3_TY = bool(__thiscall *)(HookVictim_t const *, bool) noexcept;
using FUNC4_TY = unsigned char(__thiscall *)(HookVictim_t *, array<double, 3> const &) noexcept;
using FUNC5_TY = decltype(&HookVictim_t::FUNC5);

static_assert(std::is_same_v<FUNC5_TY, decltype(&_Hook5)>);

namespace Hydrogenium::UnitTest
{
	void VirtualFnTableHook(void) noexcept
	{
		fmt::print(fg(fmt::color::lime_green), "{:-^48}\n", "Object Call");

		HookVictim_t object{};

		object.FUNC1();
		assert(object.FUNC2(123, 456.f) == 0xDEAD);
		assert(object.FUNC3(true) == true);
		assert(object.FUNC4({ 1.0, 2.0, 3.0 }) == 0x03);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "VFT Direct Call");

		std::uintptr_t *pvft = (std::uintptr_t *)&object;
		std::uintptr_t *vft = (std::uintptr_t *)*pvft;
		auto fn1 = (FUNC1_TY)vft[0];
		auto fn2 = (FUNC2_TY)vft[1];
		auto fn3 = (FUNC3_TY)vft[2];
		auto fn4 = bit_cast<FUNC4_TY>(&HookVictim_t::FUNC4);

		fn1(nullptr);
		assert(fn2(nullptr, 789, 1011.f) == 0xDEAD);
		assert(fn3(&object, true) == true);
		assert(fn4(&object, { 1.0, 2.0, 3.0 }) == 0x03);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Patched Call (1)");

		unsigned char rgPatch1[5]{}, rgOriginalBytes1[5]{};
		FUNC1_TY pfnOrg1 = nullptr;

		UTIL_PreparePatch(fn1, UTIL_CreateTrampoline(true, 0, &_Hook1), rgPatch1, rgOriginalBytes1, (void **)&pfnOrg1);
		UTIL_DoPatch(fn1, rgPatch1);

		object.FUNC1();
		fn1(nullptr);
		pfnOrg1(nullptr);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Patched Call (2)");

		unsigned char rgPatch2[5]{}, rgOriginalBytes2[5]{};
		FUNC2_TY pfnOrg2 = nullptr;

		UTIL_PreparePatch(fn2, UTIL_CreateTrampoline(true, 2, &_Hook2), rgPatch2, rgOriginalBytes2, (void **)&pfnOrg2);
		UTIL_DoPatch(fn2, rgPatch2);

		assert(object.FUNC2(42, 42.f) == 0xBEEF);
		assert(fn2(nullptr, 996, 1.048596f) == 0xBEEF);
		assert(pfnOrg2(nullptr, 512, (float)std::numbers::phi) == 0xDEAD);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Patched Call (3)");

		unsigned char rgPatch3[5]{}, rgOriginalBytes3[5]{};
		FUNC3_TY pfnOrg3 = nullptr;

		UTIL_PreparePatch(fn3, UTIL_CreateTrampoline(true, 1, &_Hook3), rgPatch3, rgOriginalBytes3, (void **)&pfnOrg3);
		UTIL_DoPatch(fn3, rgPatch3);

		assert(object.FUNC3(false) == true);
		assert(fn3(&object, false) == true);
		assert(pfnOrg3(&object, true) == true);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Patched Call (4 - mem fn)");

		unsigned char rgPatch4[5]{}, rgOriginalBytes4[5]{};
		FUNC4_TY pfnOrg4 = nullptr;

		UTIL_PreparePatch(fn4, UTIL_CreateTrampoline(true, 1, &_Hook4), rgPatch4, rgOriginalBytes4, (void **)&pfnOrg4);
		UTIL_DoPatch(fn4, rgPatch4);

		assert(object.FUNC4({ 1.0, 2.0, 3.0 }) == 0x12);
		assert(fn4(&object, { 1.0, 2.0, 3.0 }) == 0x12);
		assert(pfnOrg4(&object, { 1.0, 2.0, 3.0 }) == 0x03);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Patched Call (5 - global fn)");

		unsigned char rgPatch5[5]{}, rgOriginalBytes5[5]{};
		FUNC5_TY pfnOrg5 = nullptr;

		UTIL_PreparePatch(&HookVictim_t::FUNC5, &_Hook5, rgPatch5, rgOriginalBytes5, (void **)&pfnOrg5);
		UTIL_DoPatch(&HookVictim_t::FUNC5, rgPatch5);

		static constexpr array rgflOutput{ 1.0, 2.0, 3.0 };
		assert(HookVictim_t::FUNC5(2, 4, 6) == rgflOutput);
		assert(pfnOrg5(1, 2, 3) == rgflOutput);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Unpatch Call");

		UTIL_UndoPatch(fn1, rgOriginalBytes1);
		UTIL_UndoPatch(fn2, rgOriginalBytes2);
		UTIL_UndoPatch(fn3, rgOriginalBytes3);
		UTIL_UndoPatch(fn4, rgOriginalBytes4);
		UTIL_UndoPatch(&HookVictim_t::FUNC5, rgOriginalBytes5);

		UTIL_DisposeTrampoline(fn1, rgPatch1);
		UTIL_DisposeTrampoline(fn2, rgPatch2);
		UTIL_DisposeTrampoline(fn3, rgPatch3);
		UTIL_DisposeTrampoline(fn4, rgPatch4);

		object.FUNC1();
		assert(object.FUNC2(123, 456.f) == 0xDEAD);
		assert(object.FUNC3(true) == true);
		assert(object.FUNC4({ 1.0, 2.0, 3.0 }) == 0x03);
		assert(object.FUNC5(1, 2, 3) == rgflOutput);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Test Finished");
	}
}