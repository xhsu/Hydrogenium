module;

#pragma warning( push )
#pragma warning( disable : 4005 )
#pragma warning( disable : 5105 )
#pragma warning( disable : 5106 )

#include <cassert>

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

export module UtlHook;

import <cstdint>;

import <array>;
import <bit>;
import <vector>;

using std::array;
using std::bit_cast;
using std::vector;

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
inline void UTIL_PreparePatch(void* pTargetAddr, void* pfnReplacement, unsigned char (&rgPatch)[5], unsigned char (&rgOriginalBytes)[5], void** ppfnOriginal = nullptr) noexcept
{
	rgPatch[0] = 0xE9;	// jmp ; relative jump
	*((uint32_t *)(&rgPatch[1])) = (char *)pfnReplacement - (char *)pTargetAddr - 5;

	memcpy(rgOriginalBytes, pTargetAddr, sizeof(rgOriginalBytes));

	if (ppfnOriginal)
		*ppfnOriginal = bit_cast<void *>(*((uint32_t *)(&rgOriginalBytes[1])) + (uint32_t)pTargetAddr + 5u);
}

export
inline void UTIL_DoPatch(void *pTargetAddr, unsigned char(&rgPatch)[5]) noexcept
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
inline void UTIL_UndoPatch(void *pTargetAddr, unsigned char(&rgOriginalBytes)[5]) noexcept
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

export
inline void UTIL_DisposeTrampoline(void *pTargetAddr, unsigned char(&rgPatch)[5]) noexcept
{
	auto const pTramp = *((uint32_t *)(&rgPatch[1])) + (uint32_t)pTargetAddr + 5;

	VirtualFree((void *)pTramp, 0, MEM_RELEASE);

	memset(&rgPatch[0], 0x00, sizeof(rgPatch));
}

export template <size_t dwPatternLen>
void *MH_SearchPattern(void const *const pStartSearch, const DWORD dwSearchLen, const unsigned char(&rgszPattern)[dwPatternLen]) noexcept
{
	DWORD dwStartAddr = bit_cast<DWORD>(pStartSearch);
	const DWORD dwEndAddr = dwStartAddr + dwSearchLen - dwPatternLen;

	while (dwStartAddr < dwEndAddr)
	{
		bool found = true;

		for (DWORD i = 0; i < dwPatternLen; i++)
		{
			char const code = *(char *)(dwStartAddr + i);

			if (rgszPattern[i] != '*' && rgszPattern[i] != code)
			{
				found = false;
				break;
			}
		}

		if (found)
			return (void *)dwStartAddr;

		dwStartAddr++;
	}

	return nullptr;
}

export
inline DWORD MH_GetModuleBase(HMODULE hModule) noexcept
{
	MEMORY_BASIC_INFORMATION mem{};

	if (!VirtualQuery(hModule, &mem, sizeof(MEMORY_BASIC_INFORMATION)))
		return 0;

	return (DWORD)mem.AllocationBase;
}

export
inline DWORD MH_GetModuleSize(HMODULE hModule) noexcept
{
	return ((IMAGE_NT_HEADERS *)((DWORD)hModule + ((IMAGE_DOS_HEADER *)hModule)->e_lfanew))->OptionalHeader.SizeOfImage;
}

#pragma warning( pop )
