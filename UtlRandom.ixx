module;

#include <random>

export module UtlRandom;

import UtlConcepts;

export template<ProperIter Iter>
Iter UTIL_GetRandomOne(Iter start, Iter end, auto& g) noexcept
{
	std::uniform_int_distribution<> dis(0, std::distance(start, end) - 1);
	std::advance(start, dis(g));
	return start;
}

export auto UTIL_GetRandomOne(const Iterable auto& obj) noexcept
{
	static std::random_device rd;
	static std::mt19937 gen(rd());
	return UTIL_GetRandomOne(std::begin(obj), std::end(obj), gen);
}
