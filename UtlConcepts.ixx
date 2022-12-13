export module UtlConcepts;

export import std;

using std::any;
using std::expected;
using std::optional;
using std::shared_ptr;
using std::unique_ptr;
using std::variant;

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

template <typename... Tys>
inline constexpr bool _impl_HasSpecializedIsAsOp = false;

template <typename... Tys>
inline constexpr bool _impl_HasSpecializedIsAsOp<variant<Tys...>> = true;

template <>
inline constexpr bool _impl_HasSpecializedIsAsOp<any> = true;

template <typename T>
inline constexpr bool _impl_HasSpecializedIsAsOp<optional<T>> = true;

template <typename T, typename E>
inline constexpr bool _impl_HasSpecializedIsAsOp<expected<T, E>> = true;

template <typename T>
inline constexpr bool _impl_HasSpecializedIsAsOp<shared_ptr<T>> = true;

template <typename T, typename D>
inline constexpr bool _impl_HasSpecializedIsAsOp<unique_ptr<T, D>> = true;

template <typename T>
inline constexpr bool _impl_HasSpecializedIsAsOp<T> = std::is_pointer_v<std::remove_cvref_t<T>>;

template <typename T>
concept MustUseGeneralIsAsOp = !_impl_HasSpecializedIsAsOp<T>;

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

export template <typename T>
inline constexpr bool AlwaysFalse = false;

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

// The operator 'is' #UNTESTED
namespace Hydrogenium
{
	//export template <typename T, typename U> concept is = std::is_same_v<T, U> || (std::is_base_of_v<U, T> && std::is_convertible_v<const volatile T *, const volatile U *>);

	export template <typename T, typename... Tys> constexpr bool is(const variant<Tys...> &v) noexcept { return std::holds_alternative<T>(v); }

	export template <typename T> bool is(const any &v) noexcept { return v.type() == typeid(T); }

	export template <typename T, typename U> constexpr bool is(const optional<U> &v) noexcept { if constexpr (!std::is_same_v<T, U>) return false; else return v.has_value(); }

	export template <typename T, typename U, typename E> constexpr bool is(const expected<U, E> &v) noexcept { if constexpr (std::is_same_v<T, U>) return v.has_value(); else if constexpr (std::is_same_v<T, E>) return !v.has_value(); else return false; }

	export template <typename T, typename U> bool is(const shared_ptr<U> &ptr) noexcept { return std::dynamic_pointer_cast<T>(ptr) != nullptr; }

	export template <typename T, typename U, typename D> bool is(const unique_ptr<U, D> &ptr) noexcept { return dynamic_cast<T>(ptr.get()) != nullptr; }

	export template <typename T> constexpr bool is(auto &&ptr) noexcept requires (std::is_pointer_v<std::remove_cvref_t<decltype(ptr)>>)
	{
		using F = std::remove_cvref_t<decltype(ptr)>;
		using To_t = std::remove_pointer_t<std::remove_cvref_t<T>>;
		using From_t = std::remove_pointer_t<F>;

		if constexpr (std::derived_from<To_t, From_t>)	// downcast
			return dynamic_cast<T>(ptr) != nullptr;
		else if constexpr (std::derived_from<From_t, To_t>)	// upcast
			return true;
		else
			return std::is_same_v<std::remove_cvref_t<T>, F>;	// Same type but different cv.
	}

	export template <typename T> constexpr bool is(MustUseGeneralIsAsOp auto &&val) noexcept { return std::is_same_v<std::remove_cvref_t<decltype(val)>, std::remove_cvref_t<T>>; }
}

// The operaotr 'as' #UNTESTED
namespace Hydrogenium
{
	export template <typename T, typename... Tys> constexpr decltype(auto) as(const variant<Tys...> &v) noexcept { return std::get<T>(v); }

	export template <typename T> decltype(auto) as(const any &v) noexcept { return std::any_cast<T>(v); }

	export template <typename T, typename U> constexpr decltype(auto) as(const optional<U> &v) noexcept { if constexpr (std::is_same_v<T, U>) return *v; else return static_cast<T>(*v); }

	export template <typename T, typename U, typename E> constexpr decltype(auto) as(const expected<U, E> &v) noexcept
	{
		if constexpr (std::is_same_v<T, U>)
			return v.value();
		else if constexpr (std::is_same_v<T, E>)
			return v.error();
		else if constexpr (std::is_convertible_v<U, T>)
			return static_cast<T>(v.value());
		else if constexpr (std::is_convertible_v<E, T>)
			return static_cast<T>(v.error());
		else
			static_assert(AlwaysFalse<T>, "Illegal usage of 'as': Uncastable type from std::expected<U, E>");
	}

	export template <typename T, typename U> decltype(auto) as(const shared_ptr<U> &ptr) noexcept { return std::dynamic_pointer_cast<T>(ptr); }

	export template <typename T, typename U, typename D> decltype(auto) as(unique_ptr<U, D> &&ptr) noexcept
	{
		if (T result = dynamic_cast<T>(ptr.get()); result != nullptr)
		{
			ptr.release();
			return unique_ptr<T, D>(result, std::move(ptr.get_deleter()));
		}

		return unique_ptr<T, D>(nullptr, ptr.get_deleter());
	}

	export template <typename T> decltype(auto) as(auto &&ptr) noexcept requires (std::is_pointer_v<std::remove_cvref_t<decltype(ptr)>>)
	{
		using F = std::remove_cvref_t<decltype(ptr)>;
		using To_t = std::remove_pointer_t<std::remove_cvref_t<T>>;
		using From_t = std::remove_pointer_t<F>;

		if constexpr (std::is_same_v<std::remove_cvref_t<T>, F>)	// Same type but different cv.
			return static_cast<T>(ptr);
		else if constexpr (std::derived_from<To_t, From_t>)	// downcast
			return dynamic_cast<T>(ptr);
		else if constexpr (std::derived_from<From_t, To_t>)	// upcast
			return static_cast<T>(ptr);
		else if constexpr (std::is_same_v<F, void *> || std::is_same_v<std::remove_cvref_t<T>, void *>)	// typeless casting
			return static_cast<T>(ptr);
		else
			static_assert(AlwaysFalse<T>, "Illegal usage of 'as': 'From_t' have no safe way casting to 'To_t'.");
	}

	export template <typename T> constexpr decltype(auto) as(MustUseGeneralIsAsOp auto &&val) noexcept { return static_cast<T>(val); }
}

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