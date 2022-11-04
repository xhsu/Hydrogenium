#include <cassert>

#include <fmt/color.h>

import <cstdint>;

import <numbers>;

import UtlHook;


struct VirtualTable_t
{
	virtual void FUNC1(void) const noexcept { fmt::print("I am original func1!\n"); }
	virtual int FUNC2(int arg1, float arg2) const noexcept { fmt::print("I am original func2! [int: {}, float: {}]\n", arg1, arg2); return 0xDEAD; }
	virtual bool FUNC3(bool arg1) const noexcept { fmt::print("I am original func3! [member == {}]\n", arg1 ? m_1 : m_2); return arg1; }

	double m_1 = std::numbers::pi;
	double m_2 = std::numbers::e;
};

void _Hook1(VirtualTable_t const *pThis) noexcept { fmt::print("I am replaced func1!\n"); }
int _Hook2(VirtualTable_t const *pThis, int arg1, float arg2) noexcept { fmt::print("I am replaced func2! [int: {}, float: {}]\n", arg1, arg2); return 0xBEEF; }
bool _Hook3(VirtualTable_t const *pThis, bool arg1) noexcept { fmt::print("I am replaced func3! [member == {}]\n", arg1 ? pThis->m_1 : pThis->m_2); return !arg1; }

using FUNC1_TY = void(__thiscall *)(VirtualTable_t const *) noexcept;
using FUNC2_TY = int(__thiscall *)(VirtualTable_t const *, int, float) noexcept;
using FUNC3_TY = bool(__thiscall *)(VirtualTable_t const *, bool) noexcept;

namespace Hydrogenium::UnitTest
{
	void VirtualFnTableHook(void) noexcept
	{
		fmt::print(fg(fmt::color::lime_green), "{:-^48}\n", "Object Call");

		VirtualTable_t object{};

		object.FUNC1();
		assert(object.FUNC2(123, 456.f) == 0xDEAD);
		assert(object.FUNC3(true) == true);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "VFT Direct Call");

		std::uintptr_t *pvft = (std::uintptr_t *)&object;
		std::uintptr_t *vft = (std::uintptr_t *)*pvft;
		auto fn1 = (FUNC1_TY)vft[0];
		auto fn2 = (FUNC2_TY)vft[1];
		auto fn3 = (FUNC3_TY)vft[2];

		fn1(nullptr);
		assert(fn2(nullptr, 789, 1011.f) == 0xDEAD);
		assert(fn3(&object, true) == true);

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
	}
}