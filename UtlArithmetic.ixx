module;

// C
#include <cmath>

// C++
#include <algorithm>
#include <bit>
#include <concepts>
#include <limits>

// Static math lib
#include "gcem/include/gcem.hpp"

export module UtlArithmetic;

import UtlConcepts;

namespace Hydrogenium
{
	// Exclusively to single float.
	export constexpr float rsqrt(float x) noexcept
	{
#ifdef __SSE__
		return _mm_rsqrt_ps(_mm_set1_ps(x))[0];
#else
		/*
		* Reference:
		* Walczyk, C.J.; Moroz, L.V.; Cie´sli ´nski, J.L. Improving the Accuracy of the Fast Inverse Square Root by Modifying Newton–Raphson
		Corrections. Entropy 2021, 23, 86.
		*/

		float xhalf = 0.5f * x;
		int i = std::bit_cast<int, float>(x);
		i = 0x5f376908 - (i >> 1);

		float y = std::bit_cast<float, int>(i);
		y = y * (1.50087896f - xhalf * y * y);
		y = y * (1.50000057f - xhalf * y * y);	// Second iteration.

		return y;
#endif
	}

	export template <typename T>
	constexpr auto sqrt(T x) noexcept
	{
		if (std::is_constant_evaluated())	// #UPDATE_AT_CPP23 if consteval
		{
			return gcem::sqrt(x);
		}
		else
		{
			using D = std::decay_t<T>;

			if constexpr (std::is_same_v<D, float>)
				return std::sqrtf(x);
			else if constexpr (std::is_same_v<D, long double>)
				return std::sqrtl(x);
			else
				return std::sqrt(x);
		}
	}

	// Fuck std. Why can't they implement this simple thing?
	export template <typename... Tys> requires((std::numeric_limits<Tys>::has_quiet_NaN && ...))
	constexpr bool is_nan(const Tys&... numbers) noexcept
	{
		return ((numbers != numbers) || ...);
	}

	export template <typename... Tys>
	constexpr decltype(auto) max(Tys&&... args)
	{
		return std::max({ std::ref(args)... }).get();
	}
};
