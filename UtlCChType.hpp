/*
	Formalized at: May 28 2025
*/

#ifndef EXPORT
#define EXPORT
#endif

#if !defined(INCLUDED_IN_MODULE) || defined(__INTELLISENSE__)
#pragma once

#define HYDROGENIUM_UTL_C_CHTYPE 20250528L

#include <cctype>
#include <cwctype>

#else

import std.compat;

EXPORT inline constexpr auto HYDROGENIUM_UTL_C_CHTYPE = 20250528L;

#endif


namespace Hydrogenium::CCType
{
	// int isalnum(int c);
	EXPORT constexpr bool IsAlNum(unsigned char c) noexcept
	{
		if consteval
		{
			return
				('0' <= c && c <= '9')
				|| ('a' <= c && c <= 'z')
				|| ('A' <= c && c <= 'Z');
		}
		else
		{
			return std::isalnum(c);
		}
	}

	// int isalpha(int c);
	EXPORT constexpr bool IsAlpha(unsigned char c) noexcept
	{
		if consteval
		{
			return
				('a' <= c && c <= 'z')
				|| ('A' <= c && c <= 'Z');
		}
		else
		{
			return std::isalpha(c);
		}
	}

	//int isblank(int c);
	EXPORT constexpr bool IsBlank(unsigned char c) noexcept
	{
		if consteval
		{
			return c == '\t' || c == ' ';
		}
		else
		{
			return std::isblank(c);
		}
	}

	//int iscntrl(int c);
	EXPORT constexpr bool IsCntrl(unsigned char c) noexcept
	{
		if consteval
		{
			return
				('\x00' <= c && c <= '\x1F')
				|| c == '\x7F';
		}
		else
		{
			return std::iscntrl(c);
		}
	}

	//int isdigit(int c);
	EXPORT constexpr bool IsDigit(unsigned char c) noexcept
	{
		if consteval
		{
			return ('0' <= c && c <= '9');
		}
		else
		{
			return std::isdigit(c);
		}
	}

	//int isgraph(int c);
	EXPORT constexpr bool IsGraph(unsigned char c) noexcept
	{
		if consteval
		{
			return ('\x21' <= c && c <= '\x7E');
		}
		else
		{
			return std::isgraph(c);
		}
	}

	//int islower(int c);
	EXPORT constexpr bool IsLower(unsigned char c) noexcept
	{
		if consteval
		{
			return 'a' <= c && c <= 'z';
		}
		else
		{
			return std::islower(c);
		}
	}

	//int isprint(int c);
	EXPORT constexpr bool IsPrint(unsigned char c) noexcept
	{
		if consteval
		{
			return ('\x20' <= c && c <= '\x7E');
		}
		else
		{
			return std::isprint(c);
		}
	}

	//int ispunct(int c);
	EXPORT constexpr bool IsPunct(unsigned char c) noexcept
	{
		if consteval
		{
			return
				('\x21' <= c && c <= '\x2F')		// !"#$%&'()*+,-./
				|| ('\x3A' <= c && c <= '\x40')		// :;<=>?@
				|| ('\x5B' <= c && c <= '\x60')		// [\]^_`
				|| ('\x7B' <= c && c <= '\x7E')		// {|}~
				;
		}
		else
		{
			return std::ispunct(c);
		}
	}

	//int isspace(int c);
	EXPORT constexpr bool IsSpace(unsigned char c) noexcept
	{
		if consteval
		{
			return
				c == ' '
				|| c == '\f'
				|| c == '\n'
				|| c == '\r'
				|| c == '\t'
				|| c == '\v'
				;
		}
		else
		{
			return std::isspace(c);
		}
	}

	//int isupper(int c);
	EXPORT constexpr bool IsUpper(unsigned char c) noexcept
	{
		if consteval
		{
			return 'A' <= c && c <= 'Z';
		}
		else
		{
			return std::isupper(c);
		}
	}

	//int isxdigit(int c);
	EXPORT constexpr bool IsXDigit(unsigned char c) noexcept
	{
		if consteval
		{
			return
				('0' <= c && c <= '9')
				|| ('a' <= c && c <= 'f')
				|| ('A' <= c && c <= 'F');
		}
		else
		{
			return std::isxdigit(c);
		}
	}

	//int tolower(int c);
	EXPORT constexpr auto ToLower(unsigned char c) noexcept -> decltype(c)
	{
		if consteval
		{
			if ('A' <= c && c <= 'Z')
				return static_cast<decltype(c)>(c - 'A' + 'a');

			return c;
		}
		else
		{
			return static_cast<decltype(c)>(std::tolower(c));
		}
	}

	//int toupper(int c);
	EXPORT constexpr auto ToUpper(unsigned char c) noexcept -> decltype(c)
	{
		if consteval
		{
			if ('a' <= c && c <= 'z')
				return static_cast<decltype(c)>(c - 'a' + 'A');

			return c;
		}
		else
		{
			return static_cast<decltype(c)>(std::toupper(c));
		}

	}
}
