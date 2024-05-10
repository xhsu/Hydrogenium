#pragma once

#include <cctype>
#include <cwctype>

#include <utility>	// remove on update to 'if consteval'


namespace Hydrogenium::CCType
{
	// #UPDATE_AT_CPP23 if consteval

	// int isalnum(int c);
	constexpr bool IsAlNum(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
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
	constexpr bool IsAlpha(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
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
	constexpr bool IsBlank(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return c == '\t' || c == ' ';
		}
		else
		{
			return std::isblank(c);
		}
	}

	//int iscntrl(int c);
	constexpr bool IsCntrl(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
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
	constexpr bool IsDigit(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return ('0' <= c && c <= '9');
		}
		else
		{
			return std::isdigit(c);
		}
	}

	//int isgraph(int c);
	constexpr bool IsGraph(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return ('\x21' <= c && c <= '\x7E');
		}
		else
		{
			return std::isgraph(c);
		}
	}

	//int islower(int c);
	constexpr bool IsLower(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return 'a' <= c && c <= 'z';
		}
		else
		{
			return std::islower(c);
		}
	}

	//int isprint(int c);
	constexpr bool IsPrint(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return ('\x20' <= c && c <= '\x7E');
		}
		else
		{
			return std::isprint(c);
		}
	}

	//int ispunct(int c);
	constexpr bool IsPunct(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
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
	constexpr bool IsSpace(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
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
	constexpr bool IsUpper(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
		{
			return 'A' <= c && c <= 'Z';
		}
		else
		{
			return std::isupper(c);
		}
	}

	//int isxdigit(int c);
	constexpr bool IsXDigit(unsigned char c) noexcept
	{
		if (std::is_constant_evaluated())
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
	constexpr auto ToLower(unsigned char c) noexcept -> decltype(c)
	{
		if (std::is_constant_evaluated())
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
	constexpr auto ToUpper(unsigned char c) noexcept -> decltype(c)
	{
		if (std::is_constant_evaluated())
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
