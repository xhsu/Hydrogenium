module;

#include <Windows.h>

export module UtlWinConsole;

export import <iostream>;

import UtlConcepts;

export enum : WORD
{
	WINCON_TEXT_BLACK = 0x0,	// 0x0C0C0C fmt::color::black
	WINCON_TEXT_BLUE = 0x1,		// 0x0037DA fmt::color::medium_blue
	WINCON_TEXT_GREEN,			// 0x13A10E fmt::color::forest_green
	WINCON_TEXT_SKY_BLUE,		// 0x3A96DD fmt::color::royal_blue
	WINCON_TEXT_RED,			// 0xC50F1F fmt::color::fire_brick
	WINCON_TEXT_PLUM,			// 0x881798 fmt::color::dark_magenta
	WINCON_TEXT_GOLD,			// 0xC19C00 fmt::color::dark_golden_rod
	WINCON_TEXT_SILVER,			// 0xCCCCCC fmt::color::light_gray
	WINCON_TEXT_GRAY,			// 0x767676 fmt::color::gray
	WINCON_TEXT_CORNFLOWER_BLUE,// 0x3B78FF fmt::color::royal_blue
	WINCON_TEXT_LIME,			// 0x16C60C fmt::color::lime_green
	WINCON_TEXT_CYAN,			// 0x61D6D6 fmt::color::medium_turquoise
	WINCON_TEXT_PINK,			// 0xE74856 fmt::color::indian_red
	WINCON_TEXT_MAGENTA,		// 0xB4009E fmt::color::dark_magenta
	WINCON_TEXT_BEIGE,			// 0xF9F1A5 fmt::color::pale_golden_rod
	WINCON_TEXT_WHITE,			// 0xF2F2F2 fmt::color::white_smoke

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
static WORD s_iBackgroundColor = WINCON_BG_BLACK;

export void clear_console(void) noexcept
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	COORD tl = { 0, 0 };
	CONSOLE_SCREEN_BUFFER_INFO s;
	GetConsoleScreenBufferInfo(s_hConsole, &s);

	DWORD written, cells = s.dwSize.X * s.dwSize.Y;
	FillConsoleOutputCharacter(s_hConsole, ' ', cells, tl, &written);
	FillConsoleOutputAttribute(s_hConsole, s.wAttributes, cells, tl, &written);
	SetConsoleCursorPosition(s_hConsole, tl);
}

export void set_bg(WORD bg_color = WINCON_BG_BLACK) noexcept { s_iBackgroundColor = bg_color; }
export auto __CLRCALL_OR_CDECL black_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_BLACK; }
export auto __CLRCALL_OR_CDECL blue_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_BLUE; }
export auto __CLRCALL_OR_CDECL green_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_GREEN; }
export auto __CLRCALL_OR_CDECL sky_blue_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_SKY_BLUE; }
export auto __CLRCALL_OR_CDECL red_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_RED; }
export auto __CLRCALL_OR_CDECL plum_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_PLUM; }
export auto __CLRCALL_OR_CDECL gold_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_GOLD; }
export auto __CLRCALL_OR_CDECL silver_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_SILVER; }
export auto __CLRCALL_OR_CDECL gray_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_GRAY; }
export auto __CLRCALL_OR_CDECL cb_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_CORNFLOWER_BLUE; }
export auto __CLRCALL_OR_CDECL lime_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_LIME; }
export auto __CLRCALL_OR_CDECL cyan_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_CYAN; }
export auto __CLRCALL_OR_CDECL pink_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_PINK; }
export auto __CLRCALL_OR_CDECL magenta_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_MAGENTA; }
export auto __CLRCALL_OR_CDECL beige_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_BEIGE; }
export auto __CLRCALL_OR_CDECL white_bg(OStream auto& _Ostr) noexcept -> decltype(_Ostr) { s_iBackgroundColor = WINCON_BG_WHITE; }

export auto __CLRCALL_OR_CDECL white_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_WHITE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL blue_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_BLUE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL green_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_GREEN);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL skyblue_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_SKY_BLUE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL red_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_RED);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL plum_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_PLUM);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL gold_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_GOLD);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL silver_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_SILVER);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL gray_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_GRAY);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL cornflower_blue_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_CORNFLOWER_BLUE);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL lime_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_LIME);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL cyan_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_CYAN);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL pink_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_PINK);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL magenta_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_MAGENTA);
	return _Ostr;
}

export auto __CLRCALL_OR_CDECL beige_text(OStream auto& _Ostr) noexcept -> decltype(_Ostr)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_BEIGE);
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

export std::wostream& wcout_w(void) noexcept { return std::wcout << white_text; }
export std::wostream& wcout_b(void) noexcept { return std::wcout << blue_text; }
export std::wostream& wcout_g(void) noexcept { return std::wcout << green_text; }
export std::wostream& wcout_skyblue(void) noexcept { return std::wcout << skyblue_text; }
export std::wostream& wcout_r(void) noexcept { return std::wcout << red_text; }
export std::wostream& wcout_plum(void) noexcept { return std::wcout << plum_text; }
export std::wostream& wcout_gold(void) noexcept { return std::wcout << gold_text; }
export std::wostream& wcout_silver(void) noexcept { return std::wcout << silver_text; }
export std::wostream& wcout_gray(void) noexcept { return std::wcout << gray_text; }
export std::wostream& wcout_cb(void) noexcept { return std::wcout << cornflower_blue_text; }
export std::wostream& wcout_lime(void) noexcept { return std::wcout << lime_text; }
export std::wostream& wcout_cyan(void) noexcept { return std::wcout << cyan_text; }
export std::wostream& wcout_pink(void) noexcept { return std::wcout << pink_text; }
export std::wostream& wcout_magenta(void) noexcept { return std::wcout << magenta_text; }
export std::wostream& wcout_beige(void) noexcept { return std::wcout << beige_text; }

export struct rgb_text
{
	rgb_text(std::int8_t r, std::int8_t g, std::int8_t b) noexcept : m_PackedRGB{ RGB(r, g, b) } {}

	COLORREF m_PackedRGB{};
};

export auto __CLRCALL_OR_CDECL operator<< (OStream auto &os, rgb_text const &rhs) noexcept -> decltype(os)
{
	[[unlikely]]
	if (!s_hConsole)
		s_hConsole = GetStdHandle(STD_OUTPUT_HANDLE);

	static CONSOLE_SCREEN_BUFFER_INFOEX info{ .cbSize = sizeof(CONSOLE_SCREEN_BUFFER_INFOEX) };

	GetConsoleScreenBufferInfoEx(s_hConsole, &info);

	info.ColorTable[WINCON_TEXT_BLACK] = rhs.m_PackedRGB;

	SetConsoleScreenBufferInfoEx(s_hConsole, &info);

	SetConsoleTextAttribute(s_hConsole, s_iBackgroundColor + WINCON_TEXT_BLACK);

	return os;
}

export std::ostream &cout_rgb(std::int8_t r, std::int8_t g, std::int8_t b) noexcept { return std::cout << rgb_text(r, g, b); }
export std::wostream &wcout_rgb(std::int8_t r, std::int8_t g, std::int8_t b) noexcept { return std::wcout << rgb_text(r, g, b); }
