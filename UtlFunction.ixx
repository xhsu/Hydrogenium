module;

#include <functional>
#include <tuple>

export module UtlFunction;

export template<typename T>
struct FunctionTraits : public std::false_type {};

export template<typename R, typename ...Args>
struct FunctionTraits<R(Args...)> : public std::true_type
{
	// Reflection
	using Result_t = R;
	template<std::size_t i> using Arg_t = std::tuple_element_t<i, std::tuple<Args...>>;

	// Constants
	static constexpr size_t ArgCount = sizeof...(Args);
};

export template<typename T>
concept IsFunction = FunctionTraits<T>::value;
