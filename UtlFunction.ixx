export module UtlFunction;

export import <functional>;
export import <tuple>;

export template <typename T>
struct FunctionTraits : public std::false_type {};

export template <typename R, typename ...Args>
struct FunctionTraits<R(Args...)> : public std::true_type
{
	// Reflection
	using Result_t = R;
	template<std::size_t i> using Arg_t = std::tuple_element_t<i, std::tuple<Args...>>;

	// Constants
	static constexpr size_t ArgCount = sizeof...(Args);
};

export template <typename T>
concept IsFunction = FunctionTraits<T>::value;

// Credit: https://youtu.be/iWKewYYKPHk?t=2214
// This can be used as a functor overload set when std::visit involved. REQUIRES C++17
// Usage: auto const f = LambdaSet { lambda1, lambda2, ... };
export template <typename... Tys>
struct LambdaSet : Tys...
{
	using Tys::operator()...;
};
