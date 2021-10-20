module;

#include <functional>
#include <tuple>

export module UtlFunction;

export template<typename T>
struct FunctionTraits;

export template<typename R, typename ...Args>
struct FunctionTraits<std::function<R(Args...)>>	// #UNDONE
{
	// Reflection
	using Result_t = R;
	template<std::size_t i> using Arg_t = std::tuple_element_t<i, std::tuple<Args...>>;

	// Constants
	static constexpr size_t ArgCount = sizeof...(Args);
};
