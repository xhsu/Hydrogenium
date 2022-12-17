export module UtlRandom;

export import <random>;

import UtlConcepts;

#ifdef _M_X64
using MT19937_t = std::mt19937_64;
#else
using MT19937_t = std::mt19937;
#endif

#ifdef USING_MULTITHREAD
std::shared_mutex g_hMutexPureRNG, g_hMutexSeeded;
#endif
std::random_device g_PureRD, g_SeededRD;
MT19937_t gen(g_PureRD());

export template <Arithmetic T>
[[nodiscard]]
T UTIL_Random(T low, T high) noexcept
{
#ifdef USING_MULTITHREAD
	static std::mutex LocalMutex;
	std::scoped_lock Lock(LocalMutex);
#endif
	static MT19937_t gen(g_PureRD());

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
#ifdef USING_MULTITHREAD
	static std::mutex LocalMutex;
	std::scoped_lock Lock(LocalMutex);
#endif
	static MT19937_t gen(g_PureRD());
	return std::bernoulli_distribution()(gen);
}

export template <Arithmetic T>
[[nodiscard]]
T UTIL_SeededRandom(MT19937_t::result_type uiSeed, T low, T high) noexcept
{
#ifdef USING_MULTITHREAD
	static std::mutex LocalMutex;
	std::scoped_lock Lock(LocalMutex);
#endif
	static std::random_device rd;
	uiSeed ^= rd();
	MT19937_t gen(uiSeed);

	if constexpr (AnySame<T, short, int, long, long long, unsigned short, unsigned int, unsigned long, unsigned long long>)
		return std::uniform_int_distribution<T>(low, high)(gen);
	else if constexpr (AnySame<T, float, double, long double>)
		return std::uniform_real_distribution<T>(low, high)(gen);
	else
		static_assert(!sizeof(T), "<T> Must be a arithmetic non-boolean type!");
}

export template<ProperIter Iter>
[[nodiscard]]
Iter UTIL_GetRandomOne(Iter start, Iter end) noexcept
{
#ifdef USING_MULTITHREAD
	static std::mutex LocalMutex;
	std::scoped_lock Lock(LocalMutex);
#endif
	static MT19937_t gen(g_PureRD());

	std::uniform_int_distribution<Iter::difference_type> dis(0, std::distance(start, end) - 1);
	std::advance(start, dis(gen));
	return start;
}

export
[[nodiscard]]
decltype(auto) UTIL_GetRandomOne(const Iterable auto& obj) noexcept
{
	return *UTIL_GetRandomOne(std::begin(obj), std::end(obj));
}
