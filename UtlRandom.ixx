export module UtlRandom;

import std;

import UtlConcepts;

#ifdef _M_X64
using MT19937_t = std::mt19937_64;
#else
using MT19937_t = std::mt19937;
#endif

thread_local std::random_device g_PureRD, g_SeededRD;
thread_local MT19937_t gen(g_PureRD());

export template <typename T> requires std::is_arithmetic_v<T>
[[nodiscard]]
T UTIL_Random(T low, T high) noexcept
{
	thread_local static MT19937_t gen(g_PureRD());

	if constexpr (AnySame<T, short, int, long, long long, unsigned short, unsigned int, unsigned long, unsigned long long>)
		return std::uniform_int_distribution<T>(low, high)(gen);
	else if constexpr (AnySame<T, float, double, long double>)
		return std::uniform_real_distribution<T>(low, high)(gen);
	else
		static_assert(!sizeof(T), "<T> Must be a arithmetic non-boolean type!");
}

export
[[nodiscard]]
bool UTIL_Random(void) noexcept
{
	thread_local static MT19937_t gen(g_PureRD());
	return std::bernoulli_distribution()(gen);
}

export template <typename T> requires std::is_arithmetic_v<T>
[[nodiscard]]
T UTIL_SeededRandom(MT19937_t::result_type uiSeed, T low, T high) noexcept
{
	thread_local static std::random_device rd;
	uiSeed ^= rd();
	MT19937_t gen(uiSeed);

	if constexpr (AnySame<T, short, int, long, long long, unsigned short, unsigned int, unsigned long, unsigned long long>)
		return std::uniform_int_distribution<T>(low, high)(gen);
	else if constexpr (AnySame<T, float, double, long double>)
		return std::uniform_real_distribution<T>(low, high)(gen);
	else
		static_assert(!sizeof(T), "<T> Must be a arithmetic non-boolean type!");
}

export template <ProperIter Iter>
[[nodiscard]]
Iter UTIL_GetRandomOne(Iter start, Iter end) noexcept
{
	thread_local static MT19937_t gen(g_PureRD());

	auto const back = std::distance(start, end) - 1;
	if (back <= 0)
		return start;

	std::uniform_int_distribution<Iter::difference_type> dis(0, back);
	std::advance(start, dis(gen));
	return start;
}

export
[[nodiscard]]
auto UTIL_GetRandomOne(std::ranges::random_access_range auto&& obj) noexcept -> decltype(obj[0])
{
	thread_local static MT19937_t gen(g_PureRD());

	if constexpr (requires { obj[size_t{}]; { std::ssize(obj) } -> std::same_as<std::ptrdiff_t>; })
	{
		auto const back = std::ranges::ssize(obj) - 1;
		if (back <= 0)
			return obj[0];	// probably crash if empty.

		std::uniform_int_distribution<std::ptrdiff_t> dis(0, back);
		return obj[dis(gen)];
	}
	else
		return *UTIL_GetRandomOne(std::begin(obj), std::end(obj));
}

export inline
decltype(auto) UTIL_Shuffle(std::ranges::random_access_range auto&& obj) noexcept
{
	thread_local static MT19937_t gen(g_PureRD());

	return std::ranges::shuffle(obj, gen);
}
