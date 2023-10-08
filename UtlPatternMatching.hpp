/*
Oct. 02 2023
*/

#include <any>
#include <concepts>
#include <ranges>
#include <variant>

template <typename T> struct as_t final { using type = T; };
template <typename T> inline constexpr as_t<T> as = {};

template <typename T>
inline constexpr decltype(auto) operator/ (auto&& lhs, as_t<T>) noexcept
{
	using arg_t = std::remove_cvref_t<decltype(lhs)>;
	using decay_T = std::remove_cvref_t<T>;
	using rmptr_arg_t = std::remove_pointer_t<arg_t>;
	using rmptr_T = std::remove_pointer_t<decay_T>;

	if constexpr (requires { { std::get<T>(lhs) } -> std::common_with<T&>; })
	{
		return std::get<T>(lhs);
	}
	else if constexpr (typeid(arg_t) == typeid(std::any))
	{
		return std::any_cast<T&>(lhs);
	}
	else if constexpr (requires (arg_t e) { { e.error() } -> std::convertible_to<T&>; })
	{
		return lhs.error();
	}
	else if constexpr (requires (arg_t e) { { *e } -> std::convertible_to<T&>; })
	{
		return *lhs;
	}
	else if constexpr (std::is_base_of_v<rmptr_arg_t, rmptr_T> && !std::is_same_v<rmptr_arg_t, rmptr_T>)
	{
		return dynamic_cast<T>(lhs);
	}
	else
	{
		return static_cast<T>(lhs);
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

template <typename T>
inline constexpr bool operator/ (auto&& lhs, is_t<T>) noexcept
{
	using arg_t = std::remove_cvref_t<decltype(lhs)>;
	using decay_T = std::remove_cvref_t<T>;
	using rmptr_arg_t = std::remove_pointer_t<arg_t>;
	using rmptr_T = std::remove_pointer_t<decay_T>;

	if constexpr (requires { { std::holds_alternative<T>(lhs) } -> std::same_as<bool>; })
	{
		return std::holds_alternative<T>(lhs);
	}
	else if constexpr (typeid(arg_t) == typeid(std::any))
	{
		return lhs.type() == typeid(T);
	}
	else if constexpr (requires (arg_t e) { { e.error() } -> std::convertible_to<T&>; })
	{
		return !lhs.has_value();
	}
	else if constexpr (requires (arg_t e) { { *e } -> std::convertible_to<T&>; })
	{
		return !!lhs;
	}
	else if constexpr (std::is_base_of_v<rmptr_arg_t, rmptr_T> && !std::is_same_v<rmptr_arg_t, rmptr_T>)
	{
		if constexpr (std::is_lvalue_reference_v<decltype(lhs)> && std::is_lvalue_reference_v<T>)
		{
			try
			{
				[[maybe_unused]]
				auto& r = dynamic_cast<T>(lhs);
			}
			catch (...)
			{
				return false;
			}

			return true;
		}
		else
		{
			return dynamic_cast<T>(lhs) != nullptr;
		}
	}
	else if constexpr (std::is_base_of_v<rmptr_T, rmptr_arg_t>)
	{
		return true;
	}
	else
	{
		return typeid(lhs) == typeid(T);
	}
}

template <typename T>
inline constexpr decltype(auto) operator| (std::ranges::range auto&& lhs, is_t<T>) noexcept
{
	return lhs
		| std::views::filter([](auto&& v) noexcept -> bool { return ::operator/(std::forward<decltype(v)>(v), is<T>); });
}

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
