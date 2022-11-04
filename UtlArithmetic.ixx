export module UtlArithmetic;

// C
import <cmath>;

// C++
import <algorithm>;
import <bit>;
import <array>;
import <concepts>;
import <limits>;

// 3rd party lib
import "gcem/include/gcem.hpp";

// Friendly modules.
import UtlConcepts;

using std::array;

export namespace Hydrogenium
{
	// Ref: https://stackoverflow.com/questions/4948780/magic-number-in-boosthash-combine
	inline constexpr std::size_t UINT_MAX_OVER_PHI =
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

	// Fuck std. Why can't they implement this simple thing?
	template <typename... Tys> requires ((std::numeric_limits<Tys>::has_quiet_NaN && ...))
	[[nodiscard]] constexpr bool is_nan(Tys... numbers) noexcept
	{
		return ((numbers != numbers) || ...);
	}

	template <typename... Tys> requires ((std::numeric_limits<Tys>::has_infinity && ...))
	[[nodiscard]] constexpr bool is_inf(Tys... numbers) noexcept
	{
		return ((numbers == std::numeric_limits<Tys>::infinity() || numbers == -std::numeric_limits<Tys>::infinity()) || ...);
	}

	template <typename... Tys> requires (((std::numeric_limits<Tys>::has_infinity && std::numeric_limits<Tys>::has_quiet_NaN) && ...))
	[[nodiscard]] constexpr bool is_finite(Tys... numbers) noexcept
	{
		return ((!is_nan(numbers) && !is_inf(numbers)) || ...);
	}

	// #CONSTEXPR_MATH https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p0533r9.pdf
	template <typename T> requires (std::numeric_limits<T>::is_signed)
	[[nodiscard]] constexpr auto abs(T number) noexcept
	{
		return number == T(0) ? T(0) : number < T(0) ? -number : number;
	}

	template <std::floating_point T>
	[[nodiscard]] constexpr auto ceil(T number) noexcept
	{
		if (std::is_constant_evaluated())	// #UPDATE_AT_CPP23 if consteval
			return gcem::ceil(number);
		else
		{
			if constexpr (std::same_as<T, double>)
				return std::ceil(number);
			else if constexpr (std::same_as<T, float>)
				return std::ceilf(number);
			else if constexpr (std::same_as<T, long double>)
				return std::ceill(number);
			else
				static_assert(!sizeof(T), "Unsupport type trying to ceil.");
		}
	}

	template <std::floating_point T>
	[[nodiscard]] constexpr auto floor(T number) noexcept
	{
		if (std::is_constant_evaluated())	// #UPDATE_AT_CPP23 if consteval
			return gcem::floor(number);
		else
		{
			if constexpr (std::same_as<T, double>)
				return std::floor(number);
			else if constexpr (std::same_as<T, float>)
				return std::floorf(number);
			else if constexpr (std::same_as<T, long double>)
				return std::floorl(number);
			else
				static_assert(!sizeof(T), "Unsupport type trying to floor.");
		}
	}

	template <std::floating_point T>
	[[nodiscard]] constexpr auto trunc(T number) noexcept
	{
		if (std::is_constant_evaluated())	// #UPDATE_AT_CPP23 if consteval
			return gcem::trunc(number);
		else
		{
			if constexpr (std::same_as<T, double>)
				return std::trunc(number);
			else if constexpr (std::same_as<T, float>)
				return std::truncf(number);
			else if constexpr (std::same_as<T, long double>)
				return std::truncl(number);
			else
				static_assert(!sizeof(T), "Unsupport type trying to floor.");
		}
	}

	template <std::floating_point T>
	[[nodiscard]] constexpr auto sqrt(T number) noexcept
	{
		if (std::is_constant_evaluated())	// #UPDATE_AT_CPP23 if consteval
		{
			if (!is_finite(number))
				return number;
			else
			{
				auto fnNewtonMethod = [number](this auto &&self, T const flLastGuess) -> T // #UPDATE_AT_CPP23 static operator()
				{
					T const flNewGuess = (flLastGuess + number / flLastGuess) / static_cast<T>(2);

					if (abs(flNewGuess - flLastGuess) <= std::numeric_limits<T>::epsilon())
						return flNewGuess;

					return self(flNewGuess);
				};

				return fnNewtonMethod(number > 1 ? number / T(2) : (number * number));
			}
		}
		else
		{
			if constexpr (std::same_as<T, double>)
				return std::sqrt(number);
			else if constexpr (std::same_as<T, float>)
				return std::sqrtf(number);
			else if constexpr (std::same_as<T, long double>)
				return std::sqrtl(number);
			else
				static_assert(!sizeof(T), "Unsupport type trying to sqrt.");
		}
	}

	template <std::floating_point T>
	[[nodiscard]] constexpr auto tan(T number) noexcept
	{
		if (std::is_constant_evaluated())
		{
			constexpr array rgiNumerators{ 1.0, 1.0, 2.0, 17.0, 62.0, 1'382.0, 21'844.0, 929'569.0, 6'404'582.0, 443'861'162.0 };
			constexpr array rgiDominators{ 1.0, 3.0, 15.0, 315.0, 2'835.0, 155'925.0, 6'081'075.0, 638'512'875.0, 10'854'718'875.0, 1'856'156'927'625.0 };
			constexpr array rgiPowers{ 1, 3, 5, 7, 9, 11, 13, 15, 17, 19 };

			auto const fnIntPower = [number](unsigned const iPow) -> T
			{
				if (iPow == 0)
					return static_cast<T>(1);

				T ret = number;	// iteration 0
				for (unsigned i = 1; i < iPow; ++i)
					ret *= number;

				return ret;
			};

			auto const fnTaylorSeries = [&fnIntPower]<std::size_t... I>(std::index_sequence<I...>&&)
			{
				return (((rgiNumerators[I] * fnIntPower(rgiPowers[I])) / rgiDominators[I]) + ...);
			};

			//auto const fnRecursive = [](this auto &&self, T const flXSquared, int const iDepth) -> T
			//{
			//	T(2 * iDepth - 1) - flXSquared / self(flXSquared, iDepth + 1)
			//};

			return fnTaylorSeries(std::make_index_sequence<rgiPowers.size()>{});
		}
		else
		{
			if constexpr (std::same_as<T, double>)
				return std::tan(number);
			else if constexpr (std::same_as<T, float>)
				return std::tanf(number);
			else if constexpr (std::same_as<T, long double>)
				return std::tanl(number);
			else
				static_assert(!sizeof(T), "Unsupport type trying to std::tan().");
		}
	}

	template <typename... Tys>
	[[nodiscard]] constexpr decltype(auto) max(Tys&&... args) noexcept
	{
		return std::max({ std::ref(args)... }).get();
	}

	template <typename... Tys>
	[[nodiscard]] constexpr decltype(auto) min(Tys&&... args) noexcept
	{
		return std::min({ std::ref(args)... }).get();
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
