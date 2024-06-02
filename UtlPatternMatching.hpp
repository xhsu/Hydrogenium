/*
Oct. 02 2023
*/

#include <any>
#include <bit>
#include <future>
#include <ranges>
#include <variant>

template <typename T> struct as_t final { using type = T; };
template <typename T> inline constexpr as_t<T> as = {};

template <typename to_t>
inline constexpr decltype(auto) operator/ (auto&& lhs, as_t<to_t>) noexcept
{
	using from_t = decltype(lhs);

	using rmcvref_from_t = std::remove_cvref_t<from_t>;
	using rmcvref_to_t = std::remove_cvref_t<to_t>;

	using rmptr_from_t = std::remove_pointer_t<rmcvref_from_t>;
	using rmptr_to_t = std::remove_pointer_t<rmcvref_to_t>;

	[[maybe_unused]] constexpr bool bBitCast = requires { { std::bit_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bConstCast = requires { { const_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bDynamicCast = requires { { dynamic_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bReinterpretCast = requires { { reinterpret_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bStaticCast = requires { { static_cast<to_t>(lhs) } -> std::same_as<to_t>; };

	[[maybe_unused]] constexpr bool bBetweenPtrs = std::is_pointer_v<rmcvref_from_t> && std::is_pointer_v<rmcvref_to_t>;
	[[maybe_unused]] constexpr bool bBetweenRefs = std::is_lvalue_reference_v<from_t> && std::is_lvalue_reference_v<to_t>;

	[[maybe_unused]] constexpr bool bUniquePtr = requires { typename rmcvref_from_t::pointer; typename rmcvref_from_t::element_type; typename rmcvref_from_t::deleter_type; };
	[[maybe_unused]] constexpr bool bSharedPtr = requires { typename rmcvref_from_t::element_type; typename rmcvref_from_t::weak_type; };
	[[maybe_unused]] constexpr bool bWeakPtr = requires { typename rmcvref_from_t::element_type; { lhs.lock() } -> std::same_as<std::shared_ptr<typename rmcvref_from_t::element_type>>; };
	[[maybe_unused]] constexpr bool bSmartPtr = bUniquePtr || bSharedPtr || bWeakPtr;

	[[maybe_unused]] constexpr bool bDownCasting = std::is_base_of_v<rmptr_from_t, rmptr_to_t> && !std::is_same_v<rmptr_from_t, rmptr_to_t>;
	[[maybe_unused]] constexpr bool bUpCasting = std::is_base_of_v<rmptr_to_t, rmptr_from_t> && !std::is_same_v<rmptr_from_t, rmptr_to_t>;

	[[maybe_unused]] constexpr bool bAny = typeid(rmcvref_from_t) == typeid(std::any);
	[[maybe_unused]] constexpr bool bExpected = requires (rmcvref_from_t& e) { { e.error() } -> std::same_as<rmcvref_to_t&>; };
	[[maybe_unused]] constexpr bool bFuture = requires { { lhs.wait_for(std::chrono::seconds(0)) == std::future_status::ready } -> std::same_as<bool>; { lhs.get() } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bOptional = requires (rmcvref_from_t& o) { { o.value() } -> std::same_as<rmcvref_to_t&>; };
	[[maybe_unused]] constexpr bool bVariant = requires { { std::get<to_t>(lhs) } -> std::common_with<to_t>; };

	[[maybe_unused]] constexpr bool bSTL = bVariant || bAny || bExpected || bOptional || bFuture;
	[[maybe_unused]] constexpr bool bVoidPtrInvolved = std::same_as<rmcvref_from_t, void*> || std::same_as<rmcvref_to_t, void*>;
	[[maybe_unused]] constexpr bool bBytePtrInvolved = sizeof(rmptr_from_t) == 1 || sizeof(rmptr_to_t) == 1;
	[[maybe_unused]] constexpr bool bNullptrChecking = std::same_as<rmptr_to_t, std::nullptr_t>&& requires { { lhs == nullptr } -> std::same_as<bool>; };
	[[maybe_unused]] constexpr bool bNilOptChecking = requires { { std::make_optional(std::declval<typename rmptr_from_t::value_type>()) } -> std::same_as<rmptr_from_t>; } && std::same_as<rmptr_to_t, std::nullopt_t>;

	// Case 0: void*

	if constexpr (bVoidPtrInvolved)
	{
		static_assert(bBetweenPtrs, "void* casting must be performed between pointers!");
		return (to_t)lhs;
	}

	// Case 1: STL types - variant, any, expected, optional, future

	else if constexpr (bSTL)
	{
		if constexpr (bAny)
		{
			if constexpr (std::is_reference_v<to_t>)
				return *std::any_cast<rmcvref_to_t>(&lhs);

			else
				return std::any_cast<to_t>(lhs);
		}
		else if constexpr (bExpected)
		{
			return lhs.error();
		}
		else if constexpr (bFuture)
		{
			return lhs.get();
		}
		else if constexpr (bOptional)
		{
			return *lhs;
		}
		else if constexpr (bVariant)
		{
			return std::get<to_t>(lhs);
		}
		else
		{
			static_assert(false, "Unknown or unsupported STL type!");
		}
	}

	// Case 2: smart pointers

	else if constexpr (bSmartPtr)
	{
		static_assert(std::is_pointer_v<to_t>, "Smart pointer type must be casting with a format like raw pointer.");

		if constexpr (bUniquePtr)
		{
			static_assert(std::same_as<typename rmptr_from_t::deleter_type, std::default_delete<typename rmptr_from_t::element_type>>, "Only when default deleter is used can the unique_ptr available with 'as<>'.");

			using UniquePtr = std::unique_ptr<rmptr_to_t>;

			if (auto p = lhs.get() / as<typename UniquePtr::element_type*>)
			{
				UniquePtr ret{ p };
				lhs.release();
				return ret;
			}

			return UniquePtr{ nullptr };
		}
		else if constexpr (bSharedPtr)
		{
			using SharedPtr = std::shared_ptr<rmptr_to_t>;

			if (auto p = lhs.get() / as<typename SharedPtr::element_type*>)
				return SharedPtr{ std::forward<from_t>(lhs), p };

			return SharedPtr{ nullptr };
		}
		else if constexpr (bWeakPtr)
		{
			if (lhs.expired())
				return std::shared_ptr<rmptr_to_t>{ nullptr };

			return lhs.lock() / as<to_t>;
		}
		else
			static_assert(false, "Unsupported smart pointer type!");
	}

	// Case 3: raw pointers

	else if constexpr (bBetweenPtrs)
	{
		if constexpr (bDownCasting)
		{
			return dynamic_cast<to_t>(lhs);
		}
		else
		{
			static_assert(bStaticCast, "The two pointer types are non-convertible.");
			return static_cast<to_t>(lhs);
		}
	}

	// Case 4: references

	else if constexpr (bBetweenRefs)
	{
		if constexpr (bDownCasting)
		{
			return dynamic_cast<to_t>(lhs);
		}
		else
		{
			static_assert(bStaticCast, "The two reference types are non-convertible.");
			return static_cast<to_t>(lhs);
		}
	}

	// Case 5: general cast-able

	else
	{
		if constexpr (bConstCast)
			return const_cast<to_t>(lhs);

		else if constexpr (bStaticCast)
			return static_cast<to_t>(lhs);

		else if constexpr (bBitCast)
			return std::bit_cast<to_t>(lhs);

		else
		{
			static_assert(false, "Unsupported casting behaviour!!!");
			return reinterpret_cast<to_t>(lhs);
		}
	}
}

template <typename T>
inline constexpr decltype(auto) operator| (std::ranges::range auto&& lhs, as_t<T>) noexcept
{
	return lhs
		| std::views::transform([](auto&& v) noexcept -> decltype(auto) { return ::operator/(std::forward<decltype(v)>(v), as<T>); });
}

template <typename T> struct is_t final { using type = T; };
template <typename T> inline constexpr is_t<T> is = {};

template <typename to_t>
inline constexpr bool operator/ (auto&& lhs, is_t<to_t>) noexcept
{
	using from_t = decltype(lhs);

	using rmcvref_from_t = std::remove_cvref_t<from_t>;
	using rmcvref_to_t = std::remove_cvref_t<to_t>;

	using rmptr_from_t = std::remove_pointer_t<rmcvref_from_t>;
	using rmptr_to_t = std::remove_pointer_t<rmcvref_to_t>;

	[[maybe_unused]] constexpr bool bBitCast = requires { { std::bit_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bConstCast = requires { { const_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bDynamicCast = requires { { dynamic_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bReinterpretCast = requires { { reinterpret_cast<to_t>(lhs) } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bStaticCast = requires { { static_cast<to_t>(lhs) } -> std::same_as<to_t>; };

	[[maybe_unused]] constexpr bool bBetweenPtrs = std::is_pointer_v<rmcvref_from_t> && std::is_pointer_v<rmcvref_to_t>;
	[[maybe_unused]] constexpr bool bBetweenRefs = std::is_lvalue_reference_v<from_t> && std::is_lvalue_reference_v<to_t>;

	[[maybe_unused]] constexpr bool bUniquePtr = requires { typename rmcvref_from_t::pointer; typename rmcvref_from_t::element_type; typename rmcvref_from_t::deleter_type; };
	[[maybe_unused]] constexpr bool bSharedPtr = requires { typename rmcvref_from_t::element_type; typename rmcvref_from_t::weak_type; };
	[[maybe_unused]] constexpr bool bWeakPtr = requires { typename rmcvref_from_t::element_type; { lhs.lock() } -> std::same_as<std::shared_ptr<typename rmcvref_from_t::element_type>>; };
	[[maybe_unused]] constexpr bool bSmartPtr = bUniquePtr || bSharedPtr || bWeakPtr;

	[[maybe_unused]] constexpr bool bDownCasting = std::is_base_of_v<rmptr_from_t, rmptr_to_t> && !std::is_same_v<rmptr_from_t, rmptr_to_t>;
	[[maybe_unused]] constexpr bool bUpCasting = std::is_base_of_v<rmptr_to_t, rmptr_from_t> && !std::is_same_v<rmptr_from_t, rmptr_to_t>;

	[[maybe_unused]] constexpr bool bAny = typeid(rmcvref_from_t) == typeid(std::any);
	[[maybe_unused]] constexpr bool bExpected = requires (rmcvref_from_t& e) { { e.error() } -> std::same_as<rmcvref_to_t&>; };
	[[maybe_unused]] constexpr bool bFuture = requires { { lhs.wait_for(std::chrono::seconds(0)) == std::future_status::ready } -> std::same_as<bool>; { lhs.get() } -> std::same_as<to_t>; };
	[[maybe_unused]] constexpr bool bOptional = requires (rmcvref_from_t& o) { { o.value() } -> std::same_as<rmcvref_to_t&>; };
	[[maybe_unused]] constexpr bool bVariant = requires { { std::get<to_t>(lhs) } -> std::common_with<to_t>; };

	[[maybe_unused]] constexpr bool bSTL = bVariant || bAny || bExpected || bOptional || bFuture;
	[[maybe_unused]] constexpr bool bVoidPtrInvolved = std::same_as<rmcvref_from_t, void*> || std::same_as<rmcvref_to_t, void*>;
	[[maybe_unused]] constexpr bool bBytePtrInvolved = sizeof(rmptr_from_t) == 1 || sizeof(rmptr_to_t) == 1;
	[[maybe_unused]] constexpr bool bNullptrChecking = std::same_as<rmptr_to_t, std::nullptr_t> && requires { { lhs == nullptr } -> std::same_as<bool>; };
	[[maybe_unused]] constexpr bool bNilOptChecking = requires { { std::make_optional(std::declval<typename rmptr_from_t::value_type>()) } -> std::same_as<rmptr_from_t>; } && std::same_as<rmptr_to_t, std::nullopt_t>;

	// Case -1: compare with null

	if constexpr (bNullptrChecking)
	{
		return lhs == nullptr;
	}

	else if constexpr (bNilOptChecking)
	{
		return !lhs.has_value();
	}

	// Cast 0: void*

	else if constexpr (bVoidPtrInvolved)
	{
		static_assert(std::same_as<rmcvref_from_t, rmcvref_to_t>, "void* can never be certainly told!");
		return std::same_as<rmcvref_from_t, rmcvref_to_t>;
	}

	// Case 1: STL types - variant, any, expected, optional, future

	else if constexpr (bSTL)
	{
		if constexpr (bAny)
		{
			return lhs.type() == typeid(rmptr_to_t);
		}
		else if constexpr (bExpected)
		{
			return !lhs.has_value();
		}
		else if constexpr (bFuture)
		{
			return lhs.wait_for(std::chrono::seconds(0)) == std::future_status::ready;
		}
		else if constexpr (bOptional)
		{
			return !!lhs;
		}
		else if constexpr (bVariant)
		{
			return std::holds_alternative<to_t>(lhs);
		}
		else
		{
			static_assert(false, "Unknown or unsupported STL type!");
		}
	}

	// Case 2: smart pointers

	else if constexpr (bSmartPtr)
	{
		static_assert(std::is_pointer_v<to_t>, "Smart pointer type must be testing against raw pointer.");

		if constexpr (bUniquePtr || bSharedPtr)
		{
			return lhs.get() / is<to_t>;
		}
		else if constexpr (bWeakPtr)
		{
			return !lhs.expired() && lhs.lock().get() / is<to_t>;
		}
		else
			static_assert(false, "Unsupported smart pointer type!");
	}

	// Cast 3: pointers

	else if constexpr (bBetweenPtrs)
	{
		if constexpr (bDownCasting)
		{
			return lhs != nullptr && dynamic_cast<to_t>(lhs) != nullptr;
		}
		else if constexpr (bUpCasting)
		{
			return lhs != nullptr;
		}
		else
		{
			return lhs != nullptr && std::same_as<rmptr_from_t, rmptr_to_t>;
		}
	}

	// Cast 4: references

	else if constexpr (bBetweenRefs)
	{
		if constexpr (bDownCasting)
		{
			try
			{
				[[maybe_unused]]
				auto& r = dynamic_cast<to_t>(lhs);
			}
			catch (...)
			{
				return false;
			}

			return true;
		}
		else if constexpr (bUpCasting)
		{
			return true;
		}
		else
		{
			return std::same_as<from_t, to_t>;
		}
	}

	// Case 5: general

	else
		return std::same_as<from_t, to_t>;
}

template <typename T>
inline constexpr decltype(auto) operator| (std::ranges::range auto&& lhs, is_t<T>) noexcept
{
	return lhs
		| std::views::filter([](auto&& v) noexcept -> bool { return ::operator/(std::forward<decltype(v)>(v), is<T>); });
}

template <typename T> struct is_not_t final { using type = T; };
template <typename T> inline constexpr is_not_t<T> is_not = {};
template <typename T> inline constexpr bool operator/ (auto&& lhs, is_not_t<T>) noexcept { return !::operator/(std::forward<decltype(lhs)>(lhs), is<T>); }

template <typename T> struct where_it_is_t final { using type = T; };
template <typename T> inline constexpr where_it_is_t<T> where_it_is = {};

template <typename T>
inline constexpr decltype(auto) operator| (std::ranges::range auto&& lhs, where_it_is_t<T>) noexcept
{
	return
		lhs
		| is<T>
		| as<T>;
}

struct is_null_t final {};
inline constexpr is_null_t is_null = {};

inline constexpr bool operator/ (auto&& lhs, is_null_t) noexcept
{
	using from_t = decltype(lhs);
	using rmcvref_from_t = std::remove_cvref_t<from_t>;
	using rmptr_from_t = std::remove_pointer_t<rmcvref_from_t>;

	[[maybe_unused]] constexpr bool bHasValueFn = requires { { lhs.has_value() } -> std::same_as<bool>; };
	[[maybe_unused]] constexpr bool bMonostate = requires { { std::holds_alternative<std::monostate>(lhs) } -> std::same_as<bool>; };
	[[maybe_unused]] constexpr bool bExpected = requires { typename rmcvref_from_t::error_type; typename rmcvref_from_t::unexpected_type; };
	[[maybe_unused]] constexpr bool bFuture = requires { { lhs.wait_for(std::chrono::seconds(0)) == std::future_status::ready } -> std::same_as<bool>; };
	[[maybe_unused]] constexpr bool bNullptrCmp = requires { { lhs == nullptr } -> std::same_as<bool>; };
	[[maybe_unused]] constexpr bool bHasEmptyFn = requires { { std::ranges::empty(lhs) } -> std::same_as<bool>; };
	[[maybe_unused]] constexpr bool bHasLengthFn = requires { { lhs.length() } -> std::same_as<std::size_t>; };
	[[maybe_unused]] constexpr bool bWeakPtr = requires { { lhs.expired() } -> std::same_as<bool>; };

	// Case 1: variant, any, expected, optional, future

	if constexpr (bHasValueFn && !bExpected)
	{
		return !lhs.has_value();
	}
	else if constexpr (bHasValueFn && bExpected)
	{
		return lhs.has_value() && std::same_as<typename rmcvref_from_t::value_type, void>;	// watch this out.
	}
	else if constexpr (bMonostate)
	{
		return std::holds_alternative<std::monostate>(lhs);
	}
	else if constexpr (bFuture)
	{
		return lhs.wait_for(std::chrono::seconds(0)) != std::future_status::ready;
	}

	// Case 2: smart pointers
	// Cast 3: pointers

	else if constexpr (bWeakPtr)
	{
		return lhs.expired();
	}
	else if constexpr (bNullptrCmp)
	{
		return lhs == nullptr;
	}

	// Cast 6: ranges

	else if constexpr (bHasLengthFn)
	{
		return lhs.length() == 0;
	}
	else if constexpr (bHasEmptyFn)
	{
		return std::ranges::empty(lhs);
	}

	// Case 5: general

	else
	{
		static_assert(false, "Will never be null.");
		return false;
	}
}

inline constexpr decltype(auto) operator| (std::ranges::range auto&& lhs, is_null_t) noexcept
{
	return lhs
		| std::views::filter([](auto&& v) noexcept -> bool { return ::operator/(std::forward<decltype(v)>(v), is_null); });
}

struct is_not_null_t final {};
inline constexpr is_not_null_t is_not_null = {};
inline constexpr bool operator/ (auto&& lhs, is_not_null_t) noexcept { return !::operator/(std::forward<decltype(lhs)>(lhs), is_null); }

inline constexpr decltype(auto) operator| (std::ranges::range auto&& lhs, is_not_null_t) noexcept
{
	return lhs
		| std::views::filter([](auto&& v) noexcept -> bool { return ::operator/(std::forward<decltype(v)>(v), is_not_null); });
}
