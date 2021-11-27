module;

// C++
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
	export template<std::floating_point T> requires(std::is_same_v<T, float>)
	constexpr float rsqrt(const T& x) noexcept
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

	// Fuck std. Why can't they implement this simple thing?
	export template<typename... Tys> requires((std::numeric_limits<Tys>::has_quiet_NaN && ...))
	constexpr bool is_nan(const Tys&... numbers) noexcept
	{
		return ((numbers != numbers) || ...);
	}
};
