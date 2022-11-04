module;

#include <cassert>

#include <fmt/color.h>

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

export module UtlHook;

import <cstdint>;

import <array>;
import <bit>;
import <concepts>;
import <numbers>;
import <vector>;

using std::array;
using std::bit_cast;
using std::vector;
using std::uint8_t;
using std::uint16_t;
using std::uint32_t;

export [[nodiscard]]
inline auto UTIL_ChangeMemoryProtection(void *iAddress, unsigned int iSize, DWORD iProtectionScheme) noexcept
{
	FlushInstructionCache(GetCurrentProcess(), iAddress, iSize);

	static DWORD iSavedProtection{};
	return VirtualProtect(iAddress, iSize, iProtectionScheme, &iSavedProtection);
}

export [[nodiscard]]
inline auto UTIL_ChangeMemoryProtection(void *iAddress, unsigned int iSize, DWORD iProtectionScheme, PDWORD piOriginalProtectionScheme) noexcept
{
	FlushInstructionCache(GetCurrentProcess(), iAddress, iSize);

	return VirtualProtect(iAddress, iSize, iProtectionScheme, piOriginalProtectionScheme);
}

export [[nodiscard]]
inline void const *UTIL_RetrieveVirtualFunctionTable(void *pObject) noexcept
{
	const std::uintptr_t *const pvft = bit_cast<std::uintptr_t *>(pObject);
	const std::uintptr_t *const vft = bit_cast<std::uintptr_t *>(*pvft);

	return vft;
}

export [[nodiscard]]
inline void *UTIL_RetrieveVirtualFunction(void *const pObject, std::size_t const iIndex) noexcept
{
	const std::uintptr_t *const pvft = bit_cast<std::uintptr_t *>(pObject);
	const std::uintptr_t *const vft = bit_cast<std::uintptr_t *>(*pvft);

	return bit_cast<void *>(vft[iIndex]);
}

export [[nodiscard]]
inline void *UTIL_CreateTrampoline(bool bThiscall, int iParamCount, void *pfnReplacement) noexcept
{
	vector<unsigned char> rgSequence
	{
		// Prologue for a function
		0x55,						// push ebp
		0x89, 0xE5,					// mov ebp, esp

		// Align stack on 16 byte boundary
		0x83, 0xE4, 0xF0,			// and esp, 0xFFFFFFF0
	};

	const auto iStackNeeded = (iParamCount + bThiscall) * sizeof(void *);
	const auto iStackReserve = iStackNeeded + (16u - (iStackNeeded % 16u)) % 16u;	// ke::Align
	const auto iStackExtra = iStackReserve - iStackNeeded;

	// Stack space should fit in a byte
	assert(iStackExtra < 0xFF);

	if (iStackExtra > 0)
	{
		// Allocate stack space (8-bit) by adding to ESP
		rgSequence.append_range(array{ 0x83ui8, 0xECui8, (uint8_t)iStackExtra });	// sub esp, iStackExtra
	}

	for (auto which = iParamCount; which > 0; --which)
	{
		const auto val = which * 4 + 4;

		// Takes a paramter from the trampoline's stack and pushes it onto the target's stack.
		rgSequence.append_range(array{ 0xFF, 0x75, val });		// pushl [ebp + 'val']
	}

	if (bThiscall)
	{
		// Takes the "this" pointer from the trampoline and pushes it onto the target's stack.
		rgSequence.push_back(0x51);	// push ecx
	}

	// Call our procedure
	rgSequence.push_back(0xB8);												// move eax !missing addr!
	rgSequence.append_range(bit_cast<array<uint8_t, 4>>(pfnReplacement));	// !address! (patch up for missing addr)
	rgSequence.append_range(array{ 0xFF, 0xD0 });							// call eax

	// Adds to ESP, freeing up target stack space
	const auto iTargetStack = 4 * (iParamCount + bThiscall);
	rgSequence.append_range(array{ 0x81, 0xC4 });	// add esp !missing addr!
	rgSequence.append_range(bit_cast<array<uint8_t, 4>>(iTargetStack));	// !address! (patch up for missing addr)

	// Epilogue. Pops registers, and frees given amount of data from the stack.
	rgSequence.append_range(
		array{
			0x89, 0xEC,	// mov esp, ebp
			0x5D,		// pop ebp
			0xC2,		// retn !missing addr!
		}
	);
	rgSequence.append_range(bit_cast<array<uint8_t, 2>, uint16_t>(iParamCount * 4)); // !address! (patch up for missing addr)

	void *p = VirtualAlloc(nullptr, rgSequence.size(), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	memcpy(p, rgSequence.data(), rgSequence.size());

	return p;
};

export
inline void PreparePatch(void* pTargetAddr, void* pfnReplacement, unsigned char (&rgPatch)[5], unsigned char (&rgOriginalBytes)[5], void** ppfnOriginal = nullptr) noexcept
{
	rgPatch[0] = 0xE9;	// jmp ; relative jump
	*((uint32_t *)(&rgPatch[1])) = (char *)pfnReplacement - (char *)pTargetAddr - 5;

	memcpy(rgOriginalBytes, pTargetAddr, sizeof(rgPatch));

	if (ppfnOriginal)
		*ppfnOriginal = bit_cast<void *>(*((uint32_t *)(&rgOriginalBytes[1])) + (uint32_t)pTargetAddr + 5u);
}

export
inline void DoPatch(void *pTargetAddr, unsigned char(&rgPatch)[5]) noexcept
{
	if (UTIL_ChangeMemoryProtection(pTargetAddr, sizeof(rgPatch), PAGE_EXECUTE_READWRITE))
	{
		memcpy(pTargetAddr, (void *)rgPatch, sizeof(rgPatch));
	}
	else
	{
		assert(false && "Failure on DoPatch()");
	}
}

export
inline void UndoPatch(void *pTargetAddr, unsigned char(&rgOriginalBytes)[5]) noexcept
{
	if (UTIL_ChangeMemoryProtection(pTargetAddr, sizeof(rgOriginalBytes), PAGE_EXECUTE_READWRITE))
	{
		memcpy(pTargetAddr, rgOriginalBytes, sizeof(rgOriginalBytes));
	}
	else
	{
		assert(false && "Failure on UndoPatch()");
	}
}

namespace Hydrogenium::UnitTest
{
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

	export void VirtualFnTableHook(void) noexcept
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

		PreparePatch(fn1, UTIL_CreateTrampoline(true, 0, &_Hook1), rgPatch1, rgOriginalBytes1, (void **)&pfnOrg1);
		DoPatch(fn1, rgPatch1);

		object.FUNC1();
		fn1(nullptr);
		pfnOrg1(nullptr);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Patched Call (2)");

		unsigned char rgPatch2[5]{}, rgOriginalBytes2[5]{};
		FUNC2_TY pfnOrg2 = nullptr;

		PreparePatch(fn2, UTIL_CreateTrampoline(true, 2, &_Hook2), rgPatch2, rgOriginalBytes2, (void **)&pfnOrg2);
		DoPatch(fn2, rgPatch2);

		assert(object.FUNC2(42, 42.f) == 0xBEEF);
		assert(fn2(nullptr, 996, 1.048596f) == 0xBEEF);
		assert(pfnOrg2(nullptr, 512, std::numbers::phi) == 0xDEAD);

		fmt::print(fg(fmt::color::lime_green), "\n{:-^48}\n", "Patched Call (3)");

		unsigned char rgPatch3[5]{}, rgOriginalBytes3[5]{};
		FUNC3_TY pfnOrg3 = nullptr;

		PreparePatch(fn3, UTIL_CreateTrampoline(true, 1, &_Hook3), rgPatch3, rgOriginalBytes3, (void **)&pfnOrg3);
		DoPatch(fn3, rgPatch3);

		assert(object.FUNC3(false) == true);
		assert(fn3(&object, false) == true);
		assert(pfnOrg3(&object, true) == true);
	}
}