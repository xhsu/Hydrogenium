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

export namespace Hydrogenium
{
	// Ref: https://stackoverflow.com/questions/4948780/magic-number-in-boosthash-combine
	constexpr std::size_t UINT_MAX_OVER_PHI =
#ifdef _M_X64
		0x9e3779b97f4a7c16
#else
		0x9e3779b9
#endif
		;

	// Exclusively to single float.
	[[nodiscard]] constexpr float rsqrt(float x) noexcept
	{
#ifdef __SSE__
		return _mm_rsqrt_ps(_mm_set1_ps(x))[0];
#else
		/*
		* Reference:
		* Walczyk, C.J.; Moroz, L.V.; Cie´sli ´nski, J.L. Improving the Accuracy of the Fast Inverse Square Root by Modifying Newton–Raphson
		Corrections. Entropy 2021, 23, 86.
		*/

		float const xhalf = 0.5f * x;
		int i = std::bit_cast<int, float>(x);
		i = 0x5f376908 - (i >> 1);

		float y = std::bit_cast<float>(i);
		y = y * (1.50087896f - xhalf * y * y);
		y = y * (1.50000057f - xhalf * y * y);	// Second iteration.

		return y;
#endif
	}

	template <typename T>
	[[nodiscard]] constexpr auto sqrt(T x) noexcept
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
	template <typename... Tys> requires((std::numeric_limits<Tys>::has_quiet_NaN && ...))
	[[nodiscard]] constexpr bool is_nan(const Tys&... numbers) noexcept
	{
		return ((numbers != numbers) || ...);
	}

	template <typename... Tys>
	[[nodiscard]] constexpr decltype(auto) max(Tys&&... args) noexcept
	{
		return std::max({ std::ref(args)... }).get();
	}

	template <typename... Tys>
	[[nodiscard]] std::size_t hash(Tys&&... vals) noexcept
	{
		std::size_t ret = 0;

		const auto functors = std::make_tuple(std::hash<std::decay_t<Tys>>{}...);
		[&] <size_t... I>(std::index_sequence<I...>&&)
		{
			((ret ^= std::get<I>(functors)(vals) + UINT_MAX_OVER_PHI + (ret << 6) + (ret >> 2)), ...);
		}
		(std::index_sequence_for<Tys...>{});

		return ret;
	}

	[[nodiscard]] std::size_t hash(std::ranges::range auto&& obj) noexcept
	{
		std::size_t ret = 0;
		std::hash<std::ranges::range_value_t<decltype(obj)>> hasher{};

		for (auto const& elem : obj)
			ret ^= hasher(elem) + UINT_MAX_OVER_PHI + (ret << 6) + (ret >> 2);

		return ret;
	}
};
