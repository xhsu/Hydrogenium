module;

#include <concepts>
#include <deque>
#include <forward_list>
#include <iostream>
#include <list>
#include <map>
#include <optional>	// Could be remove on split.
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

export module UtlConcepts;

#pragma region Simple basic types

export using uint8 = unsigned __int8;
export using uint16 = unsigned __int16;
export using uint32 = unsigned __int32;
export using uint64 = unsigned __int64;

#pragma endregion Simple basic types

#pragma region String-related

export template<typename T>
concept CharacterType = std::is_same_v<T, char> /*|| std::is_same_v<T, char8_t>*/ || std::is_same_v<T, wchar_t> || std::is_same_v<T, char16_t> || std::is_same_v<T, char32_t>;

#pragma endregion String-related

#pragma region Arithmetic-related

export template<typename T>
concept Arithmetic = std::is_arithmetic_v<T>;

#pragma endregion Arithmetic-related

#pragma region Data structure related

export template<typename T>
concept ProperIter = requires(T iter) { *iter++; };

export template<typename T>
concept Iterable = ProperIter<decltype(std::begin(std::declval<T>()))> && ProperIter<decltype(std::end(std::declval<T>()))>;
/* This cannot be used in identifying built-in array. Hence it was removed.
requires(T t)
{
	{ std::begin(t) } -> ProperIter;
	{ std::end(t) } -> ProperIter;
};*/

template <typename T>
constexpr bool _impl_ResizableContainer = false;

template <typename T>
constexpr bool _impl_ResizableContainer<std::vector<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::deque<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::forward_list<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::list<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::set<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::multiset<T>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::map<T, U>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::multimap<T, U>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::unordered_set<T>> = true;

template <typename T>
constexpr bool _impl_ResizableContainer<std::unordered_multiset<T>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::unordered_map<T, U>> = true;

template <typename T, typename U>
constexpr bool _impl_ResizableContainer<std::unordered_multimap<T, U>> = true;

export template <typename T>
concept ResizableContainer = _impl_ResizableContainer<T>;

template<typename T>
struct _impl_ArrayDect : public std::false_type {};

template<typename T, size_t N>
struct _impl_ArrayDect<std::array<T, N>> : public std::true_type {};

template<typename T, size_t N>
struct _impl_ArrayDect<T[N]> : public std::true_type {};

export template<typename T>
concept Array = _impl_ArrayDect<T>::value;

export template<typename T>
concept Pointer = std::is_pointer_v<T>;

#pragma endregion Data structure related

#pragma region Template detector

template<typename T>
struct _impl_OutStreamDect : public std::false_type {};

template<typename T>
struct _impl_OutStreamDect<std::basic_ostream<T, std::char_traits<T>>> : public std::true_type {};

template<typename T>
struct _impl_OutStreamDect<std::basic_ofstream<T, std::char_traits<T>>> : public std::true_type {};

export template<typename T>
concept OStream = _impl_OutStreamDect<T>::value;

template<typename T>
struct _impl_InStreamDect : public std::false_type {};

template<typename T>
struct _impl_InStreamDect<std::basic_istream<T, std::char_traits<T>>> : public std::true_type {};

template<typename T>
struct _impl_InStreamDect<std::basic_ifstream<T, std::char_traits<T>>> : public std::true_type {};

export template<typename T>
concept IStream = _impl_InStreamDect<T>::value;

template<typename T>
struct _impl_StringDect : public std::false_type {};

template<typename T>
struct _impl_StringDect<std::basic_string<T, std::char_traits<T>, std::allocator<T>>> : public std::true_type {};

export template<typename T>
concept StlString = _impl_StringDect<T>::value;

template<typename T>
struct _impl_StringViewDect : public std::false_type {};

template<typename T>
struct _impl_StringViewDect<std::basic_string_view<T, std::char_traits<T>>> : public std::true_type {};

export template<typename T>
concept StlStringView = _impl_StringViewDect<T>::value;

export template<typename T>
concept StlStringClass = _impl_StringDect<T>::value || _impl_StringViewDect<T>::value;

#pragma endregion Template detector

#pragma region Type traits

template<typename T>
constexpr bool _impl_NonVoidType = true;

template<>
constexpr bool _impl_NonVoidType<void> = false;

export template<typename T>
concept NonVoid = _impl_NonVoidType<T>;

export template<typename T>
concept HasIndexOperator = requires(T t) { {t[std::declval<size_t>()]} -> NonVoid; };

template <typename T, typename... Tys>
constexpr bool _impl_AnySame = std::disjunction_v<std::is_same<T, Tys>...> || _impl_AnySame<Tys...>;

template <typename T>
constexpr bool _impl_AnySame<T> = false;

export template <typename T, typename... Tys>
concept AnySame = _impl_AnySame<T, Tys...>;

template <typename T, typename U>
constexpr bool _impl_IncludedInTuple = false;

template <typename T, typename... Tys>
constexpr bool _impl_IncludedInTuple<T, std::tuple<Tys...>> = _impl_AnySame<T, Tys...>;

export template <typename T, typename Tuple_t>
concept IncludedInTuple = _impl_IncludedInTuple<T, Tuple_t>;

template <typename... Tys>
struct _impl_AnyOrder;	// Undefined.

template <>
struct _impl_AnyOrder<std::tuple<>, std::tuple<>> : public std::true_type {};

template <typename... Tys> requires(sizeof...(Tys) > 0)
struct _impl_AnyOrder<std::tuple<Tys...>, std::tuple<>> : public std::false_type {};

template <typename... Tys> requires(sizeof...(Tys) > 0)
struct _impl_AnyOrder<std::tuple<>, std::tuple<Tys...>> : public std::false_type {};

template <typename T1, typename... Tys1, typename... Tys2> requires(sizeof...(Tys2) > 0)
struct _impl_AnyOrder<std::tuple<T1, Tys1...>, std::tuple<Tys2...>>
{
	static constexpr bool value = _impl_AnySame<T1, Tys2...> && std::conjunction_v<_impl_AnyOrder<std::tuple<Tys1...>, Remove_t<T1, Tys2...>>>;
};

//export template <typename... Tys>
//constexpr bool AnyOrder = false;
//
//export template <typename... Tys1, typename... Tys2>
//constexpr bool AnyOrder<std::tuple<Tys1...>, Tys2...> = _impl_AnyOrder<std::tuple<Tys1...>, std::tuple<Tys2...>>::value;
//
//export template <typename... Tys1, typename... Tys2>
//constexpr bool AnyOrder<std::tuple<Tys1...>, std::tuple<Tys2...>> = _impl_AnyOrder<std::tuple<Tys1...>, std::tuple<Tys2...>>::value;
/*
Unit Test
	static_assert(AnyOrder<std::tuple<int, float, double>, std::tuple<float, double, int>>);
	static_assert(!AnyOrder<std::tuple<char, float, double>, std::tuple<float, double, bool>>);
	static_assert(!AnyOrder<std::tuple<int, float>, std::tuple<float, double, int>>);
	static_assert(!AnyOrder<std::tuple<int, float, double>, std::tuple<float, double>>);
*/

export template <typename Tuple_t, typename... Tys>
concept AnyOrder = _impl_AnyOrder<Tuple_t, std::tuple<Tys...>>::value;

export template <typename Tuple1_t, typename Tuple2_t>
concept tuple_any_order = _impl_AnyOrder<Tuple1_t, Tuple2_t>::value;

// #UPDATE_AT_CPP23 #UPDATE_AT_CPP26
export template <typename T, std::size_t N>
concept is_tuple_element = requires (T t) { // exposition only
	typename std::tuple_element_t<N, std::remove_const_t<T>>;
	{ std::get<N>(t) } -> std::convertible_to<std::tuple_element_t<N, T>&>;
};

export template <typename T>
concept tuple_like = !std::is_reference_v<T> && requires {
	typename std::tuple_size<T>::type;
	std::same_as<decltype(std::tuple_size_v<T>), size_t>;
}&& []<std::size_t... I>(std::index_sequence<I...>)
{
	return (is_tuple_element<T, I> && ...);
}(std::make_index_sequence<std::tuple_size_v<T>>{});

export template <typename T>
concept pair_like = tuple_like<T> && std::tuple_size_v<T> == 2;

#pragma endregion Type traits

#pragma region move to sperate file: type utility
// #TODO

export template<typename... Tys>
using tuple_cat_t = std::invoke_result_t<decltype(std::tuple_cat<Tys...>), Tys...>;

export template<typename T, typename... Tys>
using Remove_t = tuple_cat_t<std::conditional_t<std::is_same_v<T, Tys>, std::tuple<>, std::tuple<Tys>>...>;

/*
Unit Test
	static_assert(std::same_as<
		Remove_t<int, int, char, int, float, int>,
		std::tuple<char, float>
	>,
	"Oops");
*/

export template<typename... Tys>
struct VariadicTemplateWrapper
{
	// Reflection
	using This_t = VariadicTemplateWrapper<Tys...>;
	using Tuple_t = std::tuple<Tys...>;
	template <std::size_t I> using type = std::tuple_element_t<I, Tuple_t>;

	// Properties
	static constexpr std::size_t npos = std::numeric_limits<std::size_t>::max();
	static constexpr std::size_t Count_v = sizeof...(Tys);
	static constexpr std::size_t Size_v = (sizeof(Tys) + ...);
	static constexpr bool AllSame_v = Count_v == 0 || Count_v == 1 || std::conjunction_v<std::is_same<type<0>, Tys>...>;
	template <typename T> static constexpr bool Exists_v = std::disjunction_v<std::is_same<T, Tys>...>;
	template <typename T> static constexpr std::size_t value = (std::is_same_v<T, Tys> +...);
	template <typename T> static constexpr std::size_t Index_v = []() consteval
	{
		if constexpr (Count_v == 0)
			return npos;	//static_assert(!sizeof(T), "Empty packed parameters!");
		else if constexpr (Count_v == 1)
			return 0;
		else if constexpr (value<T> > 1)
			static_assert(!sizeof(T), "Type which only appears once can be indexed!");
		else
		{
			constexpr std::array arr{ std::is_same_v<T, Tys>... };
			constexpr auto it = std::find(arr.cbegin(), arr.cend(), true);

			if (it == arr.cend())
				return npos;

			return (std::size_t)std::distance(arr.cbegin(), it);
		}
	}();
	template <typename... Tys2> static constexpr bool Isomer_v = AnyOrder<Tuple_t, Tys2...>;
	template <typename T> requires(requires{ typename T::Tuple_t; }) static constexpr bool Isomer_v<T> = tuple_any_order<Tuple_t, typename T::Tuple_t>;
	template <> static constexpr bool Isomer_v<> = Count_v == 0;
};

export template <typename GeneratorTy>
auto UTIL_Fountainize(GeneratorTy&& GenRange) noexcept
{
	using Ty = GeneratorTy::iterator::value_type;

	return [GenRange = std::move(GenRange), iter = GenRange.begin(), itEnd = GenRange.end()]() mutable -> std::optional<Ty>	// #UPDATE_AT_CPP23 static operator().
	{
		if (iter != itEnd)
		{
			Ty ret = *iter;
			++iter;
			return ret;
		}

		return std::nullopt;
	};
}

#pragma endregion move to sperate file: type utility