/*
	Created at: May 20 2024
*/

#pragma once

#define HYDROGENIUM_UTL_CHARCONV 20240520L

#include <limits>
#include <string>

namespace Hydrogenium::detail_charconv
{

}

template <typename T>
constexpr std::string UTIL_strfromi(T num) noexcept
{
	std::string ret{};
	ret.reserve(32);

	if (std::cmp_less(num, 0))
	{
		ret.push_back('-');
		num = -num;	// #UPDATE_AT_CPP26 sat cast

		if (std::cmp_less(num, 0)) [[unlikely]]
			num = std::numeric_limits<T>::max();
	}

	auto const fn =
		[&](this auto&& self, T num) noexcept -> void
		{
			if (num / 10 > 0)
				self(num / 10);	// Discard the tailing digit.

			ret.push_back(static_cast<char>('0' + num % 10));
		};

	fn(num);
	return ret;
}

static_assert(UTIL_strfromi(12345) == "12345");
static_assert(UTIL_strfromi(-12345) == "-12345");
