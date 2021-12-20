module;

#include <iostream>

#include <Windows.h>

export module UtlWinConsole;

import UtlConcepts;

export enum
{
	WINCON_TEXT_BLACK = 0x0,
	WINCON_TEXT_BLUE = 0x1,
	WINCON_TEXT_GREEN,
	WINCON_TEXT_SKY_BLUE,
	WINCON_TEXT_RED,
	WINCON_TEXT_PLUM,
	WINCON_TEXT_GOLD,
	WINCON_TEXT_SILVER,
	WINCON_TEXT_GRAY,
	WINCON_TEXT_CORNFLOWER_BLUE,
	WINCON_TEXT_LIME,
	WINCON_TEXT_CYAN,
	WINCON_TEXT_PINK,
	WINCON_TEXT_MAGENTA,
	WINCON_TEXT_BEIGE,
	WINCON_TEXT_WHITE,

	WINCON_BG_BLACK = 0x00,
	WINCON_BG_BLUE = 0x10,
	WINCON_BG_GREEN = 0x20,
	WINCON_BG_SKY_BLUE = 0x30,
	WINCON_BG_RED = 0x40,
	WINCON_BG_PLUM = 0x50,
	WINCON_BG_GOLD = 0x60,
	WINCON_BG_SILVER = 0x70,
	WINCON_BG_GRAY = 0x80,
	WINCON_BG_CORNFLOWER_BLUE = 0x90,
	WINCON_BG_LIME = 0xA0,
	WINCON_BG_CYAN = 0xB0,
	WINCON_BG_PINK = 0xC0,
	WINCON_BG_MAGENTA = 0xD0,
	WINCON_BG_BEIGE = 0xE0,
	WINCON_BG_WHITE = 0xF0,
};

static void* s_hConsole = nullptr;

export auto __CLRCALL_OR_CDECL white_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_WHITE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL blue_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_BLUE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL green_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_GREEN);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL skyblue_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_SKY_BLUE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL red_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_RED);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL plum_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_PLUM);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL gold_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_GOLD);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL silver_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_SILVER);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL gray_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_GRAY);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL cornflower_blue_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_CORNFLOWER_BLUE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL lime_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_LIME);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL cyan_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_CYAN);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL pink_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_PINK);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL magenta_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_MAGENTA);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL beige_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, WINCON_BG_BLACK + WINCON_TEXT_BEIGE);
	return _Ostr;
}

export std::ostream& cout_w(void) noexcept { return std::cout << white_text; }
export std::ostream& cout_b(void) noexcept { return std::cout << blue_text; }
export std::ostream& cout_g(void) noexcept { return std::cout << green_text; }
export std::ostream& cout_skyblue(void) noexcept { return std::cout << skyblue_text; }
export std::ostream& cout_r(void) noexcept { return std::cout << red_text; }
export std::ostream& cout_plum(void) noexcept { return std::cout << plum_text; }
export std::ostream& cout_gold(void) noexcept { return std::cout << gold_text; }
export std::ostream& cout_silver(void) noexcept { return std::cout << silver_text; }
export std::ostream& cout_gray(void) noexcept { return std::cout << gray_text; }
export std::ostream& cout_cb(void) noexcept { return std::cout << cornflower_blue_text; }
export std::ostream& cout_lime(void) noexcept { return std::cout << lime_text; }
export std::ostream& cout_cyan(void) noexcept { return std::cout << cyan_text; }
export std::ostream& cout_pink(void) noexcept { return std::cout << pink_text; }
export std::ostream& cout_magenta(void) noexcept { return std::cout << magenta_text; }
export std::ostream& cout_beige(void) noexcept { return std::cout << beige_text; }
