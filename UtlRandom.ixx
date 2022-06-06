module;

#include <random>
#include <shared_mutex>

export module UtlRandom;

import UtlConcepts;

std::shared_mutex g_hMutex;

auto& UTIL_GetSharedRNG(void) noexcept
{
	std::scoped_lock Lock(g_hMutex);

	static std::random_device rd;
#ifdef _M_X64
	static std::mt19937_64 gen(rd());
#else
	static std::mt19937 gen(rd());
#endif

	return gen;
}

export template<ProperIter Iter>
Iter UTIL_GetRandomOne(Iter start, Iter end) noexcept
{
	std::uniform_int_distribution<Iter::difference_type> dis(0, std::distance(start, end) - 1);
	std::advance(start, dis(UTIL_GetSharedRNG()));
	return start;
}

export auto UTIL_GetRandomOne(const Iterable auto& obj) noexcept
{
	return UTIL_GetRandomOne(std::begin(obj), std::end(obj));
}

export template <AnySame<short, int, long, long long, unsigned short, unsigned int, unsigned long, unsigned long long> T>
[[nodiscard]]
T UTIL_Random(T low, T high) noexcept
{
	return std::uniform_int_distribution<T>(low, high)(UTIL_GetSharedRNG());
}

export template <AnySame<float, double, long double> T>
[[nodiscard]]
T UTIL_Random(T low, T high) noexcept
{
	return std::uniform_real<T>(low, high)(UTIL_GetSharedRNG());
}

export bool UTIL_Random(void) noexcept
{
	return std::bernoulli_distribution()(UTIL_GetSharedRNG());
}
