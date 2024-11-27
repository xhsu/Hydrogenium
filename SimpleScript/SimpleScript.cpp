// SimpleScript.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#ifdef __INTELLISENSE__
#define _MSVC_TESTING_NVCC
#include <__msvc_all_public_headers.hpp>
#undef _MSVC_TESTING_NVCC
#elif defined __clang__
#include <algorithm>
#include <bit>
#include <bitset>
#include <charconv>
#include <expected>
#include <functional>
#include <memory>
#include <optional>
#include <print>
#include <ranges>
#include <span>
#include <string_view>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <cassert>
#include <cmath>

#define __forceinline
#else
#include <cassert>
import std.compat;
#endif

import UtlRandom;

using std::expected;
using std::move_only_function;
using std::optional;
using std::pair;
using std::shared_ptr;
using std::span;
using std::string;
using std::string_view;
using std::tuple;
using std::unordered_map;
using std::variant;
using std::vector;
using std::weak_ptr;

using namespace std::literals;

#pragma region String Util

template <size_t Length>
struct fixed_string
{
	constexpr fixed_string(const char(&arr)[Length]) noexcept { std::ranges::copy(arr, m_arr); }

	constexpr operator std::string_view() const noexcept { return m_arr; }

	char m_arr[Length]{};
};

static constexpr auto UTIL_Split(std::string_view const& s, char const* delimiters) noexcept -> std::vector<std::string_view>
{
	std::vector<std::string_view> ret{};

	for (auto lastPos = s.find_first_not_of(delimiters, 0), pos = s.find_first_of(delimiters, lastPos);
		s.npos != pos || s.npos != lastPos;
		lastPos = s.find_first_not_of(delimiters, pos), pos = s.find_first_of(delimiters, lastPos)
		)
	{
		ret.emplace_back(s.substr(lastPos, pos - lastPos));
	}

	return ret;
}

static constexpr auto UTIL_Trim(std::string_view s) noexcept -> std::string_view
{
	constexpr std::string_view DELIM = " \t\f\v\r\n";

	auto const pos1 = s.find_first_not_of(DELIM);
	if (pos1 == s.npos)
		return { s.end(), s.end() };	// Make sure all data comes from source

	s = s.substr(pos1);

	auto const pos2 = s.find_last_not_of(DELIM);
	s = s.substr(0, pos2 + 1);

	return s;
}

static_assert(UTIL_Trim("ABC") == "ABC");
static_assert(UTIL_Trim(" ABC") == "ABC");
static_assert(UTIL_Trim(" ABC ") == "ABC");
static_assert(UTIL_Trim("ABC ") == "ABC");
static_assert(UTIL_Trim(" \t") == "");
static_assert(UTIL_Trim("") == "");

using u32char = std::conditional_t<sizeof(wchar_t) == sizeof(char32_t), wchar_t, char32_t>;

enum struct CodePoint : uint_fast8_t
{
	WHOLE = 1,
	BEGIN_OF_2 = 2,
	BEGIN_OF_3 = 3,
	BEGIN_OF_4 = 4,
	MID,
	INVALID,
};

static constexpr auto UTIL_CodePointOf(char c) noexcept -> CodePoint
{
	auto const u = static_cast<uint32_t>(c);

	if (u <= 0x7F)
		return CodePoint::WHOLE;

	else if ((u & 0b111'000'00) == 0b110'000'00)
		return CodePoint::BEGIN_OF_2;

	else if ((u & 0b1111'0000) == 0b1110'0000)
		return CodePoint::BEGIN_OF_3;

	else if ((u & 0b11111'000) == 0b11110'000)
		return CodePoint::BEGIN_OF_4;

	else if ((u & 0b11'000000) == 0b10'000000)
		return CodePoint::MID;

	else
		return CodePoint::INVALID;
}

static constexpr auto UTIL_ToFullWidth(std::span<const char> arr) noexcept -> u32char
{
#ifndef _DEBUG
	if (std::ranges::empty(arr))
		return 0x110000;	// Invalid Unicode point, max val is 0x10FFFF
#else
	assert(!std::ranges::empty(arr));
#endif

	std::array const bytes{
		static_cast<uint8_t>(arr[0]),
		arr.size() > 1 ? static_cast<uint8_t>(arr[1]) : (uint8_t)0,
		arr.size() > 2 ? static_cast<uint8_t>(arr[2]) : (uint8_t)0,
		arr.size() > 3 ? static_cast<uint8_t>(arr[3]) : (uint8_t)0,
	};

	switch (UTIL_CodePointOf(bytes.front()))
	{
	case CodePoint::WHOLE:
		return static_cast<u32char>(bytes.front());

	case CodePoint::BEGIN_OF_2:
	{
		u32char ret = (bytes[0] & 0b00011111) << 6 | (bytes[1] & 0b00111111);

		if (ret < (u32char)0x80)		// Not a valid result, Wrong encoding
			ret = 0;					// Out of UTF8 bound, skip data  
		else if (ret > (u32char)0x7FF)	// Not a valid result, Wrong encoding
			ret = 0;					// Out of UTF8 bound, skip data

		return ret;
	}

	case CodePoint::BEGIN_OF_3:
	{
		u32char ret = (bytes[0] & 0b00001111) << 12 | (bytes[1] & 0b00111111) << 6 | (bytes[2] & 0b00111111);

		if (ret < (u32char)0x800)		// Not a valid result, Wrong encoding
			ret = 0;					// Out of UTF8 bound, skip data  
		else if (ret > (u32char)0xFFFF)	// Not a valid result, Wrong encoding
			ret = 0;					// Out of UTF8 bound, skip data  

		return ret;
	}

	case CodePoint::BEGIN_OF_4:
	{
		u32char ret =
			(bytes[0] & 0b00000111) << 18 | (bytes[1] & 0b00111111) << 12 | (bytes[2] & 0b00111111) << 6 | (bytes[3] & 0b00111111);

		if (ret < (u32char)0x10000)			// Not a valid result, Wrong encoding
			ret = 0;						// Out of UTF8 bound, skip data  
		else if (ret > (u32char)0x10FFFF)	// Not a valid result, Wrong encoding 
			ret = 0;						// Out of UTF8 bound, skip data  

		return ret;
	}

	default:
		assert(false);
		std::unreachable();
	}
}

#pragma endregion String Util

#pragma region Variant Util

template <typename... Args>
struct variant_cast_proxy final
{
	std::variant<Args...> v{};

	template <class... ToArgs>
	constexpr operator std::variant<ToArgs...>() noexcept
	{
		return std::visit(
			[](auto&& arg) { return std::variant<ToArgs...>{ std::forward<decltype(arg)>(arg) }; },
			std::move(v)
		);
	}
};

// Casting a variant to its superset.
// https://stackoverflow.com/questions/47203255/convert-stdvariant-to-another-stdvariant-with-super-set-of-types
template <typename... Args>
constexpr auto variant_cast(std::variant<Args...> v) noexcept -> variant_cast_proxy<Args...>
{
	return { std::move(v) };
}

#pragma endregion Variant Util



using instruction_t = move_only_function<void() const noexcept>;	// Representing an single action
using value_t = double;
using expr_t = move_only_function<value_t() const noexcept>;
using valref_t = shared_ptr<value_t>;
using strref_t = shared_ptr<string>;
using script_cell_t = variant<valref_t, value_t, expr_t, std::ptrdiff_t*, string, string_view, strref_t>;
using refobsv_t = variant<valref_t::weak_type, strref_t::weak_type>;

constexpr bool Cell_IsNumeric(auto const& a) noexcept
{
	return
		std::holds_alternative<valref_t>(a)
		|| std::holds_alternative<value_t>(a)
		|| std::holds_alternative<expr_t>(a);
}

constexpr bool Cell_IsLabel(auto const& a) noexcept
{
	return
		std::holds_alternative<std::ptrdiff_t*>(a);
}

constexpr bool Cell_IsString(auto const& a) noexcept
{
	return
		std::holds_alternative<string>(a)
		|| std::holds_alternative<string_view>(a)
		|| std::holds_alternative<strref_t>(a);
}

constexpr bool Cell_IsInput(auto const& a) noexcept
{
	return true;
}

constexpr bool Cell_IsOutput(auto const& a) noexcept
{
	return
		std::holds_alternative<valref_t>(a)
		|| std::holds_alternative<strref_t>(a);
}

// Evaluate argument into value_t
template <typename proj_t = std::identity>
struct visitor_script_cell final
{
	static inline constexpr proj_t m_proj{};

	using proj_res_t = std::remove_cvref_t<decltype(std::invoke(m_proj, value_t{}))>;

	inline constexpr auto operator()(auto&& a) const noexcept -> proj_res_t
	{
		if constexpr (requires { { *a } -> std::convertible_to<value_t>; })
		{
			return std::invoke(m_proj, *a);
		}
		else if constexpr (requires { { *a } -> std::convertible_to<string_view>; })
		{
			return operator()(*a);
		}
		else if constexpr (requires { { std::invoke(a) } -> std::convertible_to<value_t>; })
		{
			return std::invoke(m_proj, std::invoke(a));
		}
		else if constexpr (std::is_convertible_v<decltype(a), string_view>)
		{
			auto const pBegin = std::addressof(a[0]);
			auto const pEnd = pBegin + std::ranges::size(a);

			value_t ret{};
			std::from_chars(pBegin, pEnd, ret);

			return m_proj(ret);
		}
		else
		{
			return std::invoke(m_proj, a);
		}
	}
};

constexpr bool IsIdentifier(string_view s) noexcept
{
	if (s.empty())
		return false;

	bool const legit_starting =
		('a' <= s.front() && s.front() <= 'z')
		or ('A' <= s.front() && s.front() <= 'Z')
		or s.front() == '_';

	if (!legit_starting)
		return false;

	for (auto c : s | std::views::drop(1))
	{
		// Same rule in C/C++
		bool const legit =
			('0' <= c && c <= '9')
			or ('a' <= c && c <= 'z')
			or ('A' <= c && c <= 'Z')
			or c == '_';

		if (!legit)
			return false;
	}

	return true;
}

constexpr bool IsLiteral(string_view s, bool const bAllowSign = true) noexcept
{
	if (s.empty())
		return false;

	// String literal.
	if (s.front() == '"' && s.back() == '"' && s.size() >= 2)
		return true;
	if (s.front() == '\'' && s.back() == '\'' && s.size() >= 2)
		return true;

	auto const bSigned = (s.front() == '-' || s.front() == '+') && bAllowSign;
	// Kick the sign off, it's really messing things up.
	if (bSigned)
		s = s.substr(1);

	if (s.empty())	// What? only a sign was passed in?
		return false;

	bool const bHex = s.starts_with("0x") || s.starts_with("0X");
	bool const bOct = s.starts_with("0o") || s.starts_with("0O");
	bool const bBin = s.starts_with("0b") || s.starts_with("0B");
	auto const bindig_count = std::ranges::count_if(s, [](char c) noexcept { return '0' <= c && c <= '1'; });
	auto const octdig_count = std::ranges::count_if(s, [](char c) noexcept { return '0' <= c && c <= '7'; });
	auto const decdig_count = std::ranges::count_if(s, [](char c) noexcept { return '0' <= c && c <= '9'; });
	auto const hexdig_count = std::ranges::count_if(s, [](char c) noexcept { return "0123456789ABCDEFabcdef"sv.contains(c); });
	auto const dot_count = std::ranges::count(s, '.');
	auto const e_count = std::ranges::count(s, 'e') + std::ranges::count(s, 'E');
	auto const sign_count = std::ranges::count(s, '+') + std::ranges::count(s, '-');

	// It must be starting from 0-9 even if you are doing hex, as it starts as '0x'
	bool const bIsFrontDigit = '0' <= s.front() && s.front() <= '9';
	bool const bIsBackDigit = '0' <= s.back() && s.back() <= '9';

	// Filter out some obvious error.
	if (!bIsFrontDigit || dot_count > 1 || sign_count > 1)
		return false;	// Can have only one dot.

	// Integral literal.
	if (bBin && bindig_count == (std::ssize(s) - 1))
		return true;
	if (bOct && octdig_count == (std::ssize(s) - 1))
		return true;
	if (decdig_count == std::ssize(s))
		return true;
	if (bHex && hexdig_count == (std::ssize(s) - 1))
		return true;

	// Floating point literal.
	if ((e_count == 1 || dot_count == 1) && decdig_count == (std::ssize(s) - dot_count - e_count - sign_count) && bIsBackDigit)
		return true;	// floating point number must not be hex.

	return false;
}

static_assert(IsLiteral("1234") && IsLiteral("1e8"));
static_assert(IsLiteral("0xABCD"));
static_assert(!IsLiteral("0o5678"));	// Bad: oct number containing '8'
static_assert(!IsLiteral("0.1.1"));	// Bad: Version number.
static_assert(IsLiteral("-12.34e-5"));
static_assert(!IsLiteral("--12.34e-5"));	// Bad: too many signs
static_assert(!IsLiteral("-12.34ef5"));	// Bad: floating with 'f'
static_assert(!IsLiteral("1.") && !IsLiteral("1e"));	// Bad: Bad fp format.

namespace Op
{
	struct adaptor_int32 final
	{
		constexpr int32_t operator()(auto val) const noexcept
		{
			return static_cast<int32_t>(val);
		}
	};

	struct functor_dummy final
	{
		constexpr expected<expr_t, value_t> operator()(auto&&...) const noexcept
		{
			return {};
		}
	};

	template <typename operator_t, typename proj_t = std::identity>
	struct functor final
	{
		static inline constexpr operator_t m_op{};
		static inline constexpr proj_t m_proj{};
		static inline constexpr visitor_script_cell<proj_t> m_visitor{};

		constexpr expected<expr_t, value_t> operator()(auto&&... args) const noexcept
		{
			if constexpr ((... && (std::same_as<std::remove_cvref_t<decltype(args)>, value_t>)))
			{
				return std::unexpected(m_op(m_proj(args)...));
			}
			else
			{
				return
					// Ref: https://stackoverflow.com/questions/47496358/c-lambdas-how-to-capture-variadic-parameter-pack-from-the-upper-scope
					[...args{ std::move(args) }]() noexcept -> value_t
					{
						return m_op(m_visitor(args)...);
					};
			}
		}
	};

	struct functor_factorial final
	{
		constexpr expected<expr_t, value_t> operator()(value_t num) const noexcept
		{
			auto const factorial =
				[](this auto&& self, int32_t n) noexcept -> int32_t { return (n == 1 || n == 0) ? 1 : self(n - 1) * n; };

			return std::unexpected((value_t)factorial((int32_t)num));
		}

		expected<expr_t, value_t> operator()(auto arg) const noexcept
		{
			return
				[arg{ std::move(arg) }]() noexcept -> value_t
				{
					static auto const factorial =
						[](this auto&& self, int32_t n) noexcept -> int32_t { return (n == 1 || n == 0) ? 1 : self(n - 1) * n; };

					return (value_t)factorial((int32_t)visitor_script_cell {}(arg));
				};
		}
	};

	struct functor_power final
	{
		// #UPDATE_AT_CPP26 constexpr math, power
		expected<expr_t, value_t> operator()(value_t lhs, value_t rhs) const noexcept
		{
			return std::unexpected(std::pow(lhs, rhs));
		}

		expected<expr_t, value_t> operator()(auto lhs, auto rhs) const noexcept
		{
			return
				[lhs{ std::move(lhs) }, rhs{ std::move(rhs) }]() noexcept -> value_t
				{
					return std::pow(
						visitor_script_cell{}(lhs), visitor_script_cell{}(rhs)
					);
				};
		}
	};

	enum struct EAssoc : uint8_t
	{
		Undefined = 0xFF,
		Left = 1,
		Right = 0,
	};

	template <fixed_string ID, EAssoc ASSOCIATIVITY = EAssoc::Undefined, int_fast8_t PRECED = 0, uint_fast8_t ARG_COUNT = 0, typename FN = std::identity>
	struct script_operator_t final
	{
		static inline constexpr std::string_view m_id{ ID };
		static inline constexpr auto m_associativity = ASSOCIATIVITY;
		static inline constexpr auto m_preced = PRECED;
		static inline constexpr auto m_arg_count = ARG_COUNT;
		static inline constexpr auto functor = FN{};
	};

	using factorial_t = script_operator_t<"!", EAssoc::Left, 6, 1, functor_factorial>;

	using power_t = script_operator_t<"^", EAssoc::Right, 5, 2, functor_power>;

	using multiply_t = script_operator_t<"*", EAssoc::Left, 4, 2, functor<std::multiplies<>>>;
	using divide_t = script_operator_t<"/", EAssoc::Left, 4, 2, functor<std::divides<>>>;
	using modulo_t = script_operator_t<"%", EAssoc::Left, 4, 2, functor<std::modulus<>, adaptor_int32>>;	// Only int can take remainder.

	using plus_t = script_operator_t<"+", EAssoc::Left, 3, 2, functor<std::plus<>>>;
	using minus_t = script_operator_t<"-", EAssoc::Left, 3, 2, functor<std::minus<>>>;

	using assign_t = script_operator_t<"=", EAssoc::Right, 2, 2, functor_dummy>;	// Not available now.

	using comma_t = script_operator_t<",", EAssoc::Left, 1, 1, functor<std::identity>>;

	constexpr auto impl_all_op_wrapper(auto&& impl) noexcept
	{
		return impl.template operator() <
			Op::factorial_t,
			Op::power_t,
			Op::multiply_t, Op::divide_t, Op::modulo_t,
			Op::plus_t, Op::minus_t,
			Op::assign_t,
			Op::comma_t
		>();
	}

	constexpr auto Associativity(std::string_view s) noexcept -> Op::EAssoc
	{
		return impl_all_op_wrapper(
			[&]<typename... Tys>() noexcept -> Op::EAssoc
			{
				Op::EAssoc ret{ Op::EAssoc::Undefined };

				// Ref: https://stackoverflow.com/questions/46450054/retrieve-value-out-of-cascading-ifs-fold-expression
				[[maybe_unused]] auto const _
					= (... || ((Tys::m_id == s) && (void(ret = Tys::m_associativity), true)));

				return ret;
			}
		);
	}

	constexpr auto Preced(std::string_view s) noexcept -> int_fast8_t
	{
		return impl_all_op_wrapper(
			[&]<typename... Tys>() noexcept -> int_fast8_t
			{
				int_fast8_t ret{-1};

				// Ref: https://stackoverflow.com/questions/46450054/retrieve-value-out-of-cascading-ifs-fold-expression
				[[maybe_unused]] auto const _
					= (... || ((Tys::m_id == s) && (void(ret = Tys::m_preced), true)));

				return ret;
			}
		);
	}

	constexpr auto ArgCount(std::string_view s) noexcept -> uint_fast8_t
	{
		return impl_all_op_wrapper(
			[&]<typename... Tys>() noexcept -> uint_fast8_t
			{
				uint_fast8_t ret{0xFF};

				// Ref: https://stackoverflow.com/questions/46450054/retrieve-value-out-of-cascading-ifs-fold-expression
				[[maybe_unused]] auto const _
					= (... || ((Tys::m_id == s) && (void(ret = Tys::m_arg_count), true)));

				return ret;
			}
		);
	}

	constexpr auto Functor(std::string_view op, span<variant<valref_t, value_t, expr_t>> args) noexcept -> std::ranges::range_value_t<decltype(args)>
	{
		using ret_t = std::ranges::range_value_t<decltype(args)>;
		expected<expr_t, value_t> res{};

		/*
		if (op == "id")
		{
			if (typeof(op)::arg_count == 2)
				typeof(op)::functor(args[0], args[1]);
		}
		*/

		auto const impl_invoke = [&]<typename T>() noexcept
		{
			if constexpr (T::m_arg_count == 0)
				return T::functor();
			else if constexpr (T::m_arg_count == 1)
				return std::visit(T::functor, std::move(args[0]));
			else if constexpr (T::m_arg_count == 2)
				return std::visit(T::functor, std::move(args[0]), std::move(args[1]));
			else if constexpr (T::m_arg_count == 3)
				return std::visit(T::functor, std::move(args[0]), std::move(args[1]), std::move(args[2]));
			else
				static_assert(false, "Only up to 3 args supported for any operator.");
		};

		auto const impl_dispatcher = [&]<typename... Tys>() noexcept
		{
			[[maybe_unused]] auto const _ =
				(... || ((Tys::m_id == op) && (void(res = impl_invoke.template operator()<Tys>()), true)));
		};

		impl_all_op_wrapper(impl_dispatcher);

		if (res)
			return ret_t{ std::move(res).value() };
		else
			return ret_t{ res.error() };
	}
}

constexpr bool IsOperator(string_view s) noexcept
{
	if (s == "(" || s == ")")
		return true;

	auto const impl = [&]<typename... Tys>() noexcept
	{
		return (... || (Tys::m_id == s));
	};

	return Op::impl_all_op_wrapper(impl);
}

namespace Func
{
	template <typename R, typename... Args>
	consteval int32_t func_param_counter(R(*)(Args...))
	{
		return sizeof...(Args);
	}

	template <typename T, typename R, typename... Args>
	consteval int32_t func_param_counter(R(T::*)(Args...) const)
	{
		return sizeof...(Args);
	}

	template <typename T>
	consteval int32_t func_param_counter(T) requires (requires { { func_param_counter(&T::operator()) } -> std::convertible_to<int32_t>; })
	{
		return func_param_counter(&T::operator());
	}

	template <fixed_string ID, auto FN, typename proj_t = std::identity>
	struct script_fn_t final
	{
		struct functor_impl_t final
		{
			static inline constexpr auto m_fun{ FN };
			static inline constexpr visitor_script_cell<proj_t> m_visitor{};

			constexpr expected<expr_t, value_t> operator()(auto&&... args) const noexcept
			{
				if constexpr ((... && (std::same_as<std::remove_cvref_t<decltype(args)>, value_t>)))
				{
					return std::unexpected(m_fun(args...));
				}
				else
				{
					return
						// Ref: https://stackoverflow.com/questions/47496358/c-lambdas-how-to-capture-variadic-parameter-pack-from-the-upper-scope
						[...args{ std::move(args) }]() noexcept -> value_t
						{
							return m_fun(m_visitor(args)...);
						};
				}
			}
		};

		static inline constexpr std::string_view m_id{ ID };
		static inline constexpr auto functor = functor_impl_t{};
		static inline constexpr auto m_arg_count = func_param_counter(FN);
	};

	value_t log_n(value_t base, value_t x) noexcept
	{
		return std::log(x) / std::log(base);
	}

	// #UPDATE_AT_CPP23_cmath
	value_t quotient(value_t dividend, value_t divisor) noexcept
	{
		auto const res = std::div((int32_t)dividend, (int32_t)divisor);
		return res.quot;
	}

	// Basic
	using abs_t = script_fn_t<"abs", static_cast<value_t(*)(value_t)>(&std::abs)>;
	using remainder_t = script_fn_t<"remainder", static_cast<value_t(*)(value_t, value_t)>(&std::fmod)>;	// std::remainder() is pure and fully bullshit.
	using quotient_t = script_fn_t<"quotient", &quotient>;	// Like std::remainder(), std::remquo() is bullshit.
	using max_t = script_fn_t<"max", static_cast<value_t(*)(value_t, value_t)>(&std::fmax)>;
	using min_t = script_fn_t<"min", static_cast<value_t(*)(value_t, value_t)>(&std::fmin)>;
	using clamp_t = script_fn_t<"clamp", &std::clamp<value_t>>;

	// Exponential
	using pow_t = script_fn_t<"pow", static_cast<value_t(*)(value_t, value_t)>(&std::pow)>;
	using log_t = script_fn_t<"log", &log_n>;
	using sqrt_t = script_fn_t<"sqrt", static_cast<value_t(*)(value_t)>(&std::sqrt)>;
	using hypot2_t = script_fn_t<"hypot2", static_cast<value_t(*)(value_t, value_t)>(&std::hypot)>;
	using hypot3_t = script_fn_t<"hypot3", static_cast<value_t(*)(value_t, value_t, value_t)>(&std::hypot)>;

	// Trigonometric
	using sine_t = script_fn_t<"sin", static_cast<value_t(*)(value_t)>(&std::sin)>;
	using cosine_t = script_fn_t<"cos", static_cast<value_t(*)(value_t)>(&std::cos)>;
	using tangent_t = script_fn_t<"tan", static_cast<value_t(*)(value_t)>(&std::tan)>;
	using arcsine_t = script_fn_t<"arcsin", static_cast<value_t(*)(value_t)>(&std::asin)>;
	using arccosine_t = script_fn_t<"arccos", static_cast<value_t(*)(value_t)>(&std::acos)>;
	using arctangent_t = script_fn_t<"arctan", static_cast<value_t(*)(value_t, value_t)>(&std::atan2)>;

	// Rounding
	using ceil_t = script_fn_t<"ceil", static_cast<value_t(*)(value_t)>(&std::ceil)>;
	using floor_t = script_fn_t<"floor", static_cast<value_t(*)(value_t)>(&std::floor)>;
	using round_t = script_fn_t<"round", static_cast<value_t(*)(value_t)>(&std::round)>;

	// Random
	using randomf_t = script_fn_t<"randomf", &UTIL_Random<value_t>>;
	using randomi_t = script_fn_t<"randomi", &UTIL_Random<int32_t>, Op::adaptor_int32>;

	constexpr auto impl_all_fun_wrapper(auto&& impl) noexcept
	{
		return impl.template operator() <
			abs_t, remainder_t, quotient_t, max_t, min_t, clamp_t,
			pow_t, log_t, sqrt_t, hypot2_t, hypot3_t,
			sine_t, cosine_t, tangent_t, arcsine_t, arccosine_t, arctangent_t,
			ceil_t, floor_t, round_t,
			randomf_t, randomi_t
		> ();
	}

	constexpr auto ArgCount(std::string_view s) noexcept -> uint_fast8_t
	{
		return impl_all_fun_wrapper(
			[&]<typename... Tys>() noexcept -> uint_fast8_t
			{
				uint_fast8_t ret{ 0xFF };

				// Ref: https://stackoverflow.com/questions/46450054/retrieve-value-out-of-cascading-ifs-fold-expression
				[[maybe_unused]] auto const _
					= (... || ((Tys::m_id == s) && (void(ret = Tys::m_arg_count), true)));

				return ret;
			}
		);
	}

	// #UPDATE_AT_CPP23_cmath #UPDATE_AT_CPP26 constexpr math
	constexpr auto Functor(std::string_view fn_name, span<variant<valref_t, value_t, expr_t>> args) noexcept -> std::ranges::range_value_t<decltype(args)>
	{
		using ret_t = std::ranges::range_value_t<decltype(args)>;
		expected<expr_t, value_t> res{};

		auto const impl_invoke = [&]<typename T>() noexcept
		{
			if constexpr (T::m_arg_count == 0)
				return T::functor();
			else if constexpr (T::m_arg_count == 1)
				return std::visit(T::functor, std::move(args[0]));
			else if constexpr (T::m_arg_count == 2)
				return std::visit(T::functor, std::move(args[0]), std::move(args[1]));
			else if constexpr (T::m_arg_count == 3)
				return std::visit(T::functor, std::move(args[0]), std::move(args[1]), std::move(args[2]));
			else
				static_assert(false, "Only up to 3 args supported for any built-in function.");
		};

		auto const impl_dispatcher = [&]<typename... Tys>() noexcept
		{
			[[maybe_unused]] auto const _ =
				(... || ((Tys::m_id == fn_name) && (void(res = impl_invoke.template operator()<Tys>()), true)));
		};

		impl_all_fun_wrapper(impl_dispatcher);

		if (res)
			return ret_t{ std::move(res).value() };
		else
			return ret_t{ res.error() };

	}
}

constexpr bool IsFunction(string_view s) noexcept
{
	auto const impl = [&]<typename... Tys>() noexcept
	{
		return (... || (Tys::m_id == s));
	};

	return Func::impl_all_fun_wrapper(impl);
}



constexpr auto Tokenizer(string_view s) noexcept -> expected<vector<string_view>, string>
{
	// 1. Parse the string as long as possible, like pre-c++11
	// 2. Kicks off the last character then check again.

	expected<vector<string_view>, string> ret{ std::in_place };
	ret->reserve(s.size());

	bool bAllowSignOnNext = true;	// Should not being reset inter-tokens
	for (size_t pos = 0; pos < s.size(); /* Does nothing */)
	{
		auto len = s.size() - pos;
		while (len > 0)
		{
			auto const token = s.substr(pos, len);
			auto const bIsIdentifier = IsIdentifier(token);	// Function must be a valid identifier itself first. Hence no need to add IsFunction() here.
			auto const bIsLiteral = IsLiteral(token, bAllowSignOnNext);
			auto const bIsOperator = IsOperator(token);

			if (bIsIdentifier || bIsLiteral || bIsOperator)
			{
				ret->emplace_back(token);
				bAllowSignOnNext = bIsOperator;	// If it is an operator prev, then a sign is allow. Things like: x ^ -2 (x to the power of neg 2)
				break;
			}
			else if (len == 1 && " \t\f\v\r\n"sv.contains(s[pos]))
				break;	// space gets skipped without considered as token.

			--len;
		}

		if (!len)
			// The segment was problematically.
			return std::unexpected(std::format("Unrecognized symbol '{}' found at pos {}", s.substr(pos, 1), pos));
		else
			// If parsed, something must be inserted.
			pos += len;
	}

	return ret;
}

constexpr auto ShuntingYardAlgorithm(span<string_view const> tokens) noexcept -> expected<vector<string_view>, string>
{
	vector<string_view> ret{};
	vector<string_view> op_stack{};

	for (auto&& token : tokens)
	{
		bool const bIsLiteral = IsLiteral(token);
		bool const bIsIdentifier = IsIdentifier(token);
		bool const bIsFunction = IsFunction(token);

		// Is number?
		if ((bIsLiteral || bIsIdentifier) && !bIsFunction)
		{
			ret.push_back(token);
		}

		// Is a function?
		else if (bIsFunction)
		{
			op_stack.push_back(token);
		}

		// operator?
		else if (token[0] != '(' && token[0] != ')' && IsOperator(token))
		{
			auto const o1_preced = Op::Preced(token);

			/*
			while (
				there is an operator o2 at the top of the operator stack which is not a left parenthesis,
				and (o2 has greater precedence than o1 or (o1 and o2 have the same precedence and o1 is left-associative))
			):
				pop o2 from the operator stack into the output queue
			push o1 onto the operator stack
			*/

			while (!op_stack.empty() && op_stack.back() != "("
				&& (Op::Preced(op_stack.back()) > o1_preced || (Op::Preced(op_stack.back()) == o1_preced && Op::Associativity(token) == Op::EAssoc::Left))
				)
			{
				ret.push_back(op_stack.back());
				op_stack.pop_back();
			}

			op_stack.push_back(token);
		}

		// (
		else if (token.length() == 1 && token[0] == '(')
			op_stack.push_back("(");

		// )
		else if (token.length() == 1 && token[0] == ')')
		{
			try
			{
				while (op_stack.back() != "(")
				{
					// { assert the operator stack is not empty }
					assert(!op_stack.empty());

					// pop the operator from the operator stack into the output queue

					ret.emplace_back(op_stack.back());
					op_stack.pop_back();
				}

				assert(op_stack.back()[0] == '(');
				// pop the left parenthesis from the operator stack and discard it
				op_stack.pop_back();
			}
			catch (...)
			{
				/* If the stack runs out without finding a left parenthesis, then there are mismatched parentheses. */
				return std::unexpected("Mismatched parentheses");
			}

			/*
			if there is a function token at the top of the operator stack, then:
				pop the function from the operator stack into the output queue
			*/
			if (!op_stack.empty() && IsFunction(op_stack.back()))
			{
				ret.push_back(op_stack.back());
				op_stack.pop_back();
			}
		}

		else
			return std::unexpected("Unreconsized symbol");
	}

	/* After the while loop, pop the remaining items from the operator stack into the output queue. */
	while (!op_stack.empty())
	{
		if (op_stack.back() == "(")
			return std::unexpected("If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses");

		ret.emplace_back(op_stack.back());
		op_stack.pop_back();
	}

	return std::move(ret);	// For constructing expected<> object
}

constexpr bool UnitTest_ArithmeticExpr() noexcept
{
	auto const rpn =
		Tokenizer("3+4*2/(1-5)^2^3")
		.and_then(&ShuntingYardAlgorithm);

	auto const concat =
		*rpn
		| std::views::join
		| std::ranges::to<string>();

	return concat == "342*15-23^^/+";
}
static_assert(UnitTest_ArithmeticExpr());



struct Inspector final
{
	static inline constexpr auto fnParamOptional = [](string_view const& s) noexcept { return s.length() > 2 && s.front() == '[' && s.back() == ']'; };
	static inline constexpr auto fnParamVariadic = [](string_view const& s) noexcept { return s.ends_with("...") || s.ends_with("...]"); };

	static constexpr auto ArgCount(span<string_view const> signature) noexcept -> expected<pair<uint8_t, uint8_t>, string_view>
	{
		// %param => 1, 1
		// %param, [%opt] => 1, 2
		// %param, %var... => 2, 255
		// %param, [%opt...] => 1, 255
		// %param, %var..., %param => ERROR
		// %param, [%opt], %param => ERROR
		// %param, [%opt...], [%opt] => ERROR
		// %param, %var..., [%opt...] => ERROR

		auto const itFirstVariadic = std::ranges::find_if(signature, fnParamVariadic);
		auto const rgLastVariadic = std::ranges::find_last_if(signature, fnParamVariadic);
		if (itFirstVariadic != rgLastVariadic.begin())
			return std::unexpected("Bad signature: exactly one variadic parameter pack is allowed");
		if (rgLastVariadic.size() > 1)
			return std::unexpected("Bad signature: Variadic parameter pack must be the last one in signature");

		bool const bVariadicPresent = itFirstVariadic != signature.end();
		auto const itFirstOptional = std::ranges::find_if(signature, fnParamOptional);
		if (itFirstOptional == signature.end())	// i.e. no optional param
			return pair{ (uint8_t)signature.size(), (uint8_t)(bVariadicPresent ? 0xFF : signature.size()) };

		auto const itLastCompulsory = std::ranges::find_last_if_not(signature, fnParamOptional).begin();
		if (itLastCompulsory >= itFirstOptional)
			return std::unexpected("Bad signature: optional arguments must not come after compulsory arguments");

		return pair{ (uint8_t)(itLastCompulsory - signature.begin() + 1), (uint8_t)(bVariadicPresent ? 0xFF : signature.size()) };
	}

/*
	constexpr std::array TEST_1 = { "%param"sv };
	constexpr std::array TEST_2 = { "%param"sv, "[%opt]"sv, };
	constexpr std::array TEST_3 = { "%param"sv, "%var..."sv, };
	constexpr std::array TEST_4 = { "%param"sv, "[%opt...]"sv, };

	static_assert(Inspector::ArgCount(TEST_1).value() == pair{ 1, 1 });
	static_assert(Inspector::ArgCount(TEST_2).value() == pair{ 1, 2 });
	static_assert(Inspector::ArgCount(TEST_3).value() == pair{ 2, 255 });
	static_assert(Inspector::ArgCount(TEST_4).value() == pair{ 1, 255 });
*/

	static constexpr bool VerifyArg(string_view const& param, auto const& arg) noexcept
	{
		vector<bool> res{};
		if (param.contains("%in"))
			res.push_back(Cell_IsInput(arg));
		if (param.contains("%out"))
			res.push_back(Cell_IsOutput(arg));
		if (param.contains("%label"))
			res.push_back(Cell_IsLabel(arg));
		if (param.contains("%num"))
			res.push_back(Cell_IsNumeric(arg));
		if (param.contains("%str"))
			res.push_back(Cell_IsString(arg));

		return std::ranges::count(res, true) == std::ssize(res);
	}
};

struct error_t final
{
	constexpr error_t(error_t const&) noexcept = default;
	constexpr error_t(error_t&&) noexcept = default;
	constexpr ~error_t() noexcept = default;

	constexpr error_t& operator=(error_t const&) noexcept = default;
	constexpr error_t& operator=(error_t&&) noexcept = default;

	constexpr error_t(string_view str) noexcept
		: m_Text{ UTIL_Trim(str) }, m_Underscore(m_Text.size(), ' ')
	{
		for (size_t pos = 0; pos < m_Text.size(); /* Does nothing */)
		{
			auto len = m_Text.size() - pos;
			while (len > 0)
			{
				auto const token = m_Text.substr(pos, len);
				bool const bIsExpr = token.front() == '[' && token.back() == ']';
				bool const bIsSeparator = len == 1 && ", \t\f\v\r\n"sv.contains(token.front());
				bool const bIsLabel = len > 1 && token.back() == ':';

				if (bIsExpr || bIsLabel || IsIdentifier(token) || IsLiteral(token))	// Kind of doggy and bug-proning.
				{
					m_SegmentsText.emplace_back(token);
					m_SegmentsUnderline.emplace_back(span{ m_Underscore.data() + pos, len });
					break;
				}
				else if (bIsSeparator)
					break;	// space gets skipped without considered as token.

				--len;
			}

			if (!len)
			{
				// The segment was problematically.
				// But we are already the error reporting module, hence we have to continue.
				m_SegmentsText.emplace_back(m_Text.substr(pos));
				m_SegmentsUnderline.emplace_back(span{ m_Underscore.data() + pos, m_SegmentsText.back().length() });
				break;
			}
			else
				// If parsed, something must be inserted.
				pos += len;
		}

		assert(m_SegmentsText.size() == m_SegmentsUnderline.size());
	}

	constexpr error_t(string_view line, std::ptrdiff_t idx, string what) noexcept
		: error_t(line)
	{
		ErrorAt(idx, std::move(what));
	}

	constexpr error_t(string_view line, std::ptrdiff_t idx) noexcept
		: error_t(line)
	{
		Emphasis(idx);
	}

	constexpr void Emphasis(std::ptrdiff_t idx) noexcept
	{
		m_SegmentsUnderline[idx].front() = '^';

		for (auto& c : m_SegmentsUnderline[idx] | std::views::drop(1))
			c = '~';
	}

	constexpr void Underline(std::ptrdiff_t idx, char ch = '~') noexcept
	{
		std::ranges::fill(m_SegmentsUnderline[idx], ch);
	}

	constexpr void ErrorAt(std::ptrdiff_t idx, string what) noexcept
	{
		Emphasis(idx);
		m_ErrorMsg = std::move(what);
	}

	auto ToString(string_view leading = "") const noexcept -> string
	{
		assert(m_Text.size() == m_Underscore.size());

		return std::format(
			"{2}{0}\n{2}{1}",
			m_Text, m_Underscore,
			leading
		);
	}

	auto ToString(size_t iSpaceCount, std::ptrdiff_t line_num) const noexcept -> string
	{
		assert(m_Text.size() == m_Underscore.size());

		return std::format(
			"{0:>{4}} | {1}\n{2} | {3}",
			line_num, m_Text,
			string(iSpaceCount, ' '), m_Underscore,
			iSpaceCount
		);
	}

	constexpr auto GetText() const noexcept -> string_view const& { return m_Text; }
	constexpr auto GetUnderscore() const noexcept -> string const& { return m_Underscore; }
	constexpr auto GetTextSegment(std::ptrdiff_t idx) const noexcept -> string_view { return m_SegmentsText.at(idx); }
	constexpr auto GetUnderscoreSegment(std::ptrdiff_t idx) const noexcept -> string_view { return string_view{ m_SegmentsUnderline[idx].data(), m_SegmentsUnderline[idx].size() }; }
	constexpr auto GetSegmentCount() const noexcept -> std::ptrdiff_t { assert(m_SegmentsText.size() == m_SegmentsUnderline.size()); return std::ranges::ssize(m_SegmentsText); }

	string m_ErrorMsg{};

private:
	string_view m_Text{};
	string m_Underscore{};
	vector<string_view> m_SegmentsText{};
	vector<span<char>> m_SegmentsUnderline{};
};

struct flag_register_t final
{
	bool m_OF : 1 {};	// overflow
	bool m_DF : 1 {};	// direction
	bool m_IF : 1 {};	// interrupt
	bool m_TF : 1 {};	// trap
	bool m_SF : 1 {};	// sign
	bool m_ZF : 1 {};	// zero
	bool m_AF : 1 {};	// aux carry flag
	bool m_PF : 1 {};	// parity flag
	bool m_CF : 1 {};	// carry flag

	constexpr void Fill(bool bValue) noexcept
	{
		m_OF = bValue;
		m_DF = bValue;
		m_IF = bValue;
		m_TF = bValue;
		m_SF = bValue;
		m_ZF = bValue;
		m_AF = bValue;
		m_PF = bValue;
		m_CF = bValue;
	}

	constexpr bool Equal() const noexcept { return m_ZF; }
	constexpr bool NotEqual() const noexcept { return !m_ZF; }
	constexpr bool Greater() const noexcept { return !m_ZF && !m_SF; }
	constexpr bool GreaterOrEq() const noexcept { return Greater() || Equal(); }
	constexpr bool Lesser() const noexcept { return !m_ZF && m_SF; }
	constexpr bool LesserOrEq() const noexcept { return Lesser() || Equal(); }
};

struct script_t final
{
	constexpr script_t(script_t const&) noexcept = delete;	// not copyable
	constexpr script_t(script_t&&) noexcept = default;
	constexpr ~script_t() noexcept = default;

	constexpr script_t& operator=(script_t const&) noexcept = delete;	// not copyable
	constexpr script_t& operator=(script_t&&) noexcept = default;

	script_t() noexcept = default;
	explicit script_t(std::string_view SourceText) noexcept { Compile(SourceText); }

	// Script registers

	vector<instruction_t> m_Instructions{};
	valref_t m_eax{ std::make_shared<value_t>() };	// Accumulator register
	valref_t m_ebx{ std::make_shared<value_t>() };	// Base register
	valref_t m_ecx{ std::make_shared<value_t>() };	// Counter register
	valref_t m_edx{ std::make_shared<value_t>() };	// Data register

	shared_ptr<std::ptrdiff_t> m_eip{ std::make_shared<std::ptrdiff_t>() };	// Instruction Pointer
	shared_ptr<vector<std::ptrdiff_t>> m_esp{ std::make_shared<vector<std::ptrdiff_t>>() };	// Stack Pointer register
	// shared_ptr<uint32_t> m_ebp{ std::make_shared<uint32_t>() };	// Stack Base Pointer register

	strref_t m_esi{ std::make_shared<string>() };	// Destination Index register
	strref_t m_edi{ std::make_shared<string>() };	// Source Index register

	shared_ptr<flag_register_t> m_eflags{ std::make_shared<flag_register_t>() };
	unordered_map<string, std::ptrdiff_t, std::hash<string_view>, std::equal_to<>> m_Labels{};
	unordered_map<string, refobsv_t, std::hash<string_view>, std::equal_to<>> m_Observer{};

	// Script parser

	static constexpr auto Tokenizer_Instruction(string_view s) noexcept -> expected<vector<string_view>, error_t>
	{
		// Same tokenizer must be used in error_t
		expected<vector<string_view>, error_t> ret{ std::in_place };
		ret->reserve(s.size());

		for (size_t pos = 0; pos < s.size(); /* Does nothing */)
		{
			auto len = s.size() - pos;
			while (len > 0)
			{
				auto const token = s.substr(pos, len);
				bool const bIsExpr = token.front() == '[' && token.back() == ']';
				bool const bIsSeparator = len == 1 && ", \t\f\v\r\n"sv.contains(token.front());
				bool const bIsLabel = len > 1 && token.back() == ':';

				if (bIsExpr || bIsLabel || IsIdentifier(token) || IsLiteral(token))	// Kind of doggy and bug-proning.
				{
					ret->emplace_back(token);
					break;
				}
				else if (bIsSeparator)
					break;	// space gets skipped without considered as token.

				--len;
			}

			if (!len)
				// The segment was problematically.
				return std::unexpected(error_t{ s, std::ssize(*ret), std::format("Unrecognized symbol '{}' found at pos {}", s.substr(pos, 1), pos)});
			else
				// If parsed, something must be inserted.
				pos += len;
		}

		return ret;
	}

	__forceinline static auto Parser_GetImmediate(string_view argument) noexcept -> value_t
	{
		if (argument == "e")
			return std::numbers::e;
		if (argument == "pi" || argument == u8"π")
			return std::numbers::pi;
		if (argument == "phi" || argument == u8"ϕ")
			return std::numbers::phi;

		// Convert to unicode point, as a hex number.
		if (argument.size() <= 6 && argument.front() == '\'' && argument.back() == '\'')
			return (value_t)UTIL_ToFullWidth(argument.substr(1, argument.size() - 2));

		int base = 10;

		if (argument.starts_with("0x") || argument.starts_with("0X"))
			base = 16;
		if (argument.starts_with("0o") || argument.starts_with("0O"))
			base = 8;
		if (argument.starts_with("0b") || argument.starts_with("0B"))
			base = 2;

		if (base != 10)
		{
			int32_t ret{};	// Plus 2 to skip the 0* part
			if (std::from_chars(argument.data() + 2, argument.data() + argument.size(), ret, base).ec == std::errc{})
				return (value_t)ret;

			return std::numeric_limits<value_t>::quiet_NaN();
		}

		value_t ret{};
		if (std::from_chars(argument.data(), argument.data() + argument.size(), ret).ec == std::errc{})
			return ret;

		return std::numeric_limits<value_t>::quiet_NaN();
	}

	auto Parser_ParamNumeric(string_view argument) const noexcept -> expected<variant<valref_t, value_t, expr_t>, string>
	{
		// Handle references
		if (argument == "EAX")
			return m_eax;
		if (argument == "EBX")
			return m_ebx;
		if (argument == "ECX")
			return m_ecx;
		if (argument == "EDX")
			return m_edx;
		if (argument == "EIP")
			return std::unexpected("Instruction Pointer ought not to be accessed");
		if (argument == "ESI")
			return std::unexpected("Register ESI cannot be used in arithmetic context");
		if (argument == "EDI")
			return std::unexpected("Register EDI cannot be used in arithmetic context");

		// Handle expr
		if (argument.length() && argument.front() == '[' && argument.back() == ']')
		{
			argument = argument.substr(1, argument.size() - 2);
			auto const PostfixNotation =
				Tokenizer(argument)
				.and_then(&ShuntingYardAlgorithm);

			if (!PostfixNotation)
				return std::unexpected(std::move(PostfixNotation).error());

			// eval postfix notation expr
			vector<variant<valref_t, value_t, expr_t>> num_stack{};

			for (auto&& token : PostfixNotation.value())
			{
				bool const bIsLiteral = IsLiteral(token);
				bool const bIsIdentifier = IsIdentifier(token);
				bool const bIsFunction = IsFunction(token);

				if ((bIsLiteral || bIsIdentifier) && !bIsFunction)	// Because all function names are also legit identifier names.
				{
					auto parsed_input = Parser_ParamNumeric(token);
					if (!parsed_input)
						return std::unexpected(std::move(parsed_input).error());

					num_stack.emplace_back(std::move(parsed_input).value());
				}
				else if (IsOperator(token))
				{
					assert(token != "(" && token != ")");	// Something must be wrong if parenthesis pass through SYA.

					auto const arg_count = Op::ArgCount(token);
					auto const first_arg_pos = std::min(num_stack.size(), num_stack.size() - arg_count);
					auto const args = span{ num_stack.data() + first_arg_pos, std::min<size_t>(arg_count, num_stack.size()) };

					auto res = Op::Functor(token, args);

					num_stack.erase(num_stack.begin() + first_arg_pos, num_stack.end());
					num_stack.push_back(std::move(res));
				}

				// Function is considered as a part of input.
				else if (bIsFunction)
				{
					auto const arg_count = Func::ArgCount(token);
					if (arg_count > num_stack.size())
						return std::unexpected(std::format("Insufficient argument to invoke function '{}'. Expect {} but {} received", token, arg_count, num_stack.size()));

					auto const first_arg_pos = std::min(num_stack.size(), num_stack.size() - arg_count);
					auto const args = span{ num_stack.data() + first_arg_pos, arg_count };

					auto res = Func::Functor(token, args);

					num_stack.erase(num_stack.begin() + first_arg_pos, num_stack.end());
					num_stack.push_back(std::move(res));
				}
				else
					return std::unexpected("Unrecognized token found in expression");
			}

			if (num_stack.empty())
				return std::unexpected("Bad expression");

			// For invoking move constructor of variant<>
			return std::move(num_stack.front());
		}

		// Handle literals (Immediate)
		if (auto const val = Parser_GetImmediate(argument); val == val)	// NaN here is no found
			return val;

		return std::unexpected(std::format("'{}' is not a register, immediate number or expression", argument));
	}

	auto Parser_ParamString(string_view argument) const noexcept -> expected<variant<string, string_view, strref_t>, string>
	{
		// Handle references
		if (argument == "EAX")
			return std::unexpected("Register EAX cannot be used in string context");
		if (argument == "EBX")
			return std::unexpected("Register EBX cannot be used in string context");
		if (argument == "ECX")
			return std::unexpected("Register ECX cannot be used in string context");
		if (argument == "EDX")
			return std::unexpected("Register EDX cannot be used in string context");
		if (argument == "EIP")
			return std::unexpected("Instruction Pointer ought not to be accessed");
		if (argument == "ESI")
			return m_esi;
		if (argument == "EDI")
			return m_edi;

		// Double-quoted string literals
		if (argument.length() && argument.front() == '"' && argument.back() == '"')
			return string{ argument.substr(1, argument.size() - 2) };

		// Single-quoted character literals
		if (argument.length() <= 6 && argument.front() == '\'' && argument.back() == '\'')
			return string{ argument.substr(1, argument.size() - 2) };

		return std::unexpected(std::format("'{}' cannot be evaluated as a string", argument));
	}

	auto Parser_ProcArg(span<string_view const> signature, pair<uint8_t, uint8_t> argc, span<string_view const> arguments, string_view szLineText) noexcept -> expected<vector<script_cell_t>, vector<error_t>>
	{
		vector<script_cell_t> processed{};
		vector<error_t> errors{};
		auto const result_inserter = [&](auto&& a) noexcept { processed.emplace_back(std::forward<decltype(a)>(a)); };

		// Verify arg count
		if (arguments.size() > argc.second)
		{
			errors.emplace_back(szLineText, 0, std::format("Expected up to {} operand(s) but {} received", argc.second, arguments.size()));
			return std::unexpected(std::move(errors));
		}
		else if (arguments.size() < argc.first)
		{
			errors.emplace_back(szLineText, 0, std::format("Expected at least {} operand(s) but {} received", argc.first, arguments.size()));
			return std::unexpected(std::move(errors));
		}

		// Build a parameter list from signature
		vector<string_view> parameters{ std::from_range, signature };
		parameters.reserve(argc.second);

		// Assume the signature is legit. Which means if parameter pack presented, it must be at the last spot.
		if (Inspector::fnParamVariadic(parameters.back()))
		{
			auto const count = std::ssize(arguments) - std::ssize(parameters);
			if (count > 0)
				parameters.append_range(std::views::repeat(parameters.back()) | std::views::take(count));
		}
		assert(parameters.size() >= arguments.size());

		for (int i = 1; i < std::ssize(arguments); ++i)
		{
			expected<script_cell_t, string> res{ std::unexpect, "Internal Compiler Error: Bad parameter signature" };

			if (parameters[i].contains("%num"))
			{
				if (auto num_res = Parser_ParamNumeric(arguments[i]); num_res)
					res = variant_cast(std::move(num_res).value());
				else
					res = std::unexpected(std::move(num_res).error());
			}
			else if (parameters[i].contains("%label"))
			{
				// Creating the label here as well, in case it's forward referenced.
				auto [it, bNew] = m_Labels.try_emplace(decltype(m_Labels)::key_type{ arguments[i] }, -1);
				res = std::addressof(it->second);
			}
			else if (parameters[i].contains("%str"))
			{
				if (auto str_res = Parser_ParamString(arguments[i]); str_res)
					res = variant_cast(std::move(str_res).value());
				else
					res = std::unexpected(std::move(str_res).error());
			}

			// Checker
			if (res && !Inspector::VerifyArg(parameters[i], *res))
				errors.emplace_back(szLineText, i, "The argument does not meet the parameter constraint");
			else if (res)
				std::visit(result_inserter, std::move(res).value());
			else
				errors.emplace_back(szLineText, i, std::move(res).error());
		}

		if (!errors.empty())
			return std::unexpected(std::move(errors));

		return std::move(processed);	// Move into std::expected<>
	}

	// Data Transfer Instructions

	static inline constexpr std::string_view SIG_MOV[] = { "MOV", "%num%out", "%num%in" };
	static inline constexpr std::string_view SIG_LEA[] = { "LEA", "%num%out", "%num%in" };
	auto Parser_MOV(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_dest = arguments[0];
		auto& parsed_src = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_dest)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(parsed_src)) && !(std::holds_alternative<string>(parsed_src)));

		switch (parsed_src.index())
		{
		case 0:	// storage
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<0>(std::move(parsed_src)) }]() noexcept
				{
					*dest = *src;
				};
		case 1:	// immediate
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<1>(std::move(parsed_src)) }]() noexcept
				{
					*dest = src;
				};
		case 2:	// expression
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<2>(std::move(parsed_src)) }]() noexcept
				{
					*dest = std::invoke(src);
				};

		default:
			std::unreachable();
			break;
		}
	}

	static inline constexpr std::string_view SIG_XCHG[] = { "XCHG", "%num%in%out", "%num%in%out" };
	auto Parser_XCHG(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_lhs = arguments[0];
		auto& parsed_rhs = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_lhs)));
		assert((std::holds_alternative<valref_t>(parsed_rhs)));

		return
			[lhs{ std::get<0>(parsed_lhs) }, rhs{ std::get<0>(parsed_rhs) }]() noexcept
			{
				std::swap(*lhs, *rhs);
			};
	}

	static inline constexpr std::string_view SIG_CMPXCHG[] = { "CMPXCHG", "%num%in%out", "%num%in" };
	auto Parser_CMPXCHG(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_arg1 = arguments[0];
		auto& parsed_arg2 = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_arg1)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(parsed_arg2)) && !(std::holds_alternative<string>(parsed_arg2)));

		switch (parsed_arg2.index())
		{
		case 0:
			return
				[arg1{ std::get<0>(parsed_arg1) }, arg2{ std::get<0>(parsed_arg2) }, eflags{ m_eflags }, eax{ m_eax }]() noexcept
				{
					static constexpr auto I32_MAX = std::numeric_limits<int32_t>::max();
					static constexpr auto I32_MIN = std::numeric_limits<int32_t>::min();

					auto const diff = *arg1 - *eax;
					eflags->m_ZF = std::abs(diff) < 1e-5;
					eflags->m_SF = diff < 0;
					eflags->m_CF = false;	// For unsigned situation
					eflags->m_OF = I32_MAX < diff || diff < I32_MIN;
					eflags->m_PF = int32_t(diff) % 2 == 0;
					eflags->m_AF = false;	// LUNA: I have no idea how to set the AF.

					if (eflags->m_ZF)
						*arg1 = *arg2;
					else
						*eax = *arg1;
				};

		case 1:
			return
				[arg1{ std::get<0>(parsed_arg1) }, arg2{ std::get<1>(parsed_arg2) }, eflags{ m_eflags }, eax{ m_eax }]() noexcept
				{
					static constexpr auto I32_MAX = std::numeric_limits<int32_t>::max();
					static constexpr auto I32_MIN = std::numeric_limits<int32_t>::min();

					auto const diff = *arg1 - *eax;
					eflags->m_ZF = std::abs(diff) < 1e-5;
					eflags->m_SF = diff < 0;
					eflags->m_CF = false;	// For unsigned situation
					eflags->m_OF = I32_MAX < diff || diff < I32_MIN;
					eflags->m_PF = int32_t(diff) % 2 == 0;
					eflags->m_AF = false;	// LUNA: I have no idea how to set the AF.

					if (eflags->m_ZF)
						*arg1 = arg2;
					else
						*eax = *arg1;
				};

		case 2:
			return
				[arg1{ std::get<0>(parsed_arg1) }, arg2{ std::get<2>(std::move(parsed_arg2)) }, eflags{ m_eflags }, eax{ m_eax }]() noexcept
				{
					static constexpr auto I32_MAX = std::numeric_limits<int32_t>::max();
					static constexpr auto I32_MIN = std::numeric_limits<int32_t>::min();

					auto const diff = *arg1 - *eax;
					eflags->m_ZF = std::abs(diff) < 1e-5;
					eflags->m_SF = diff < 0;
					eflags->m_CF = false;	// For unsigned situation
					eflags->m_OF = I32_MAX < diff || diff < I32_MIN;
					eflags->m_PF = int32_t(diff) % 2 == 0;
					eflags->m_AF = false;	// LUNA: I have no idea how to set the AF.

					if (eflags->m_ZF)
						*arg1 = std::invoke(arg2);
					else
						*eax = *arg1;
				};

		default:
			std::unreachable();
		}
	}

	static inline constexpr std::string_view SIG_MOVS[] = { "MOVS", "%str%out", "%str%in" };
	auto Parser_MOVS(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_dest = arguments[0];
		auto& parsed_src = arguments[1];

		assert(std::holds_alternative<strref_t>(parsed_dest));

		if (std::holds_alternative<string>(parsed_src))
		{
			return
				[dest{ std::get<strref_t>(std::move(parsed_dest)) }, src{ std::get<string>(std::move(parsed_src)) }]() noexcept
				{
					*dest = src;
				};
		}
		else if (std::holds_alternative<string_view>(parsed_src))
		{
			return
				[dest{ std::get<strref_t>(std::move(parsed_dest)) }, src{ string{ std::get<string_view>(parsed_src) } }]() noexcept
				{
					*dest = src;
				};
		}
		else if (std::holds_alternative<strref_t>(parsed_src))
		{
			return
				[dest{ std::get<strref_t>(std::move(parsed_dest)) }, src{ std::get<strref_t>(std::move(parsed_src)) }]() noexcept
				{
					*dest = *src;
				};
		}

		assert(false);
		std::unreachable();
		return []() noexcept {};
	}

	static inline constexpr std::string_view SIG_CMOVE[] = { "CMOVE", "%num%out", "%num%in" };
	static inline constexpr std::string_view SIG_CMOVNE[] = { "CMOVNE", "%num%out", "%num%in" };
	static inline constexpr std::string_view SIG_CMOVG[] = { "CMOVG", "%num%out", "%num%in" };
	static inline constexpr std::string_view SIG_CMOVGE[] = { "CMOVGE", "%num%out", "%num%in" };
	static inline constexpr std::string_view SIG_CMOVL[] = { "CMOVL", "%num%out", "%num%in" };
	static inline constexpr std::string_view SIG_CMOVLE[] = { "CMOVLE", "%num%out", "%num%in" };

	template <auto fnCondition>
	auto Parser_CMOV(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_dest = arguments[0];
		auto& parsed_src = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_dest)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(parsed_src)) && !(std::holds_alternative<string>(parsed_src)));

		switch (parsed_src.index())
		{
		case 0:	// storage
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<0>(std::move(parsed_src)) }, eflags{ m_eflags }]() noexcept
				{
					if (std::invoke(fnCondition, *eflags))
						*dest = *src;
				};
		case 1:	// immediate
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<1>(std::move(parsed_src)) }, eflags{ m_eflags }]() noexcept
				{
					if (std::invoke(fnCondition, *eflags))
						*dest = src;
				};
		case 2:	// expression
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<2>(std::move(parsed_src)) }, eflags{ m_eflags }]() noexcept
				{
					if (std::invoke(fnCondition, *eflags))
						*dest = std::invoke(src);
				};

		default:
			std::unreachable();
			break;
		}
	}

	// Control Flow Instructions

	static inline constexpr std::string_view SIG_TEST[] = { "TEST", "%num%in", "%num%in" };
	auto Parser_TEST(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& accumulator = arguments[0];
		auto& reference = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert(!(std::holds_alternative<std::ptrdiff_t*>(accumulator)) && !(std::holds_alternative<string>(accumulator)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(reference)) && !(std::holds_alternative<string>(reference)));

		if (std::holds_alternative<value_t>(accumulator) && std::holds_alternative<value_t>(reference))
		{
			auto const lhs = std::bit_cast<uint32_t>((int32_t)std::get<value_t>(accumulator));
			auto const rhs = std::bit_cast<uint32_t>((int32_t)std::get<value_t>(reference));
			std::bitset<32> const bits{ lhs & rhs };

			return
				[bits, eflags{ m_eflags }]() noexcept
				{
					eflags->m_SF = bits[31];
					eflags->m_ZF = bits.to_ulong() == 0;
					eflags->m_PF = !(bits.count() % 2);	// Flags set if count of 'true' is even number.
					eflags->m_CF = false;
					eflags->m_OF = false;
					eflags->m_AF = false;	// Technically undefined.
				};
		}

		return
			[accu{ std::move(accumulator) }, refe{ std::move(reference) }, eflags{ m_eflags }]() noexcept
			{
				auto const lhs = std::bit_cast<uint32_t>(std::visit(visitor_script_cell<Op::adaptor_int32>{}, accu));
				auto const rhs = std::bit_cast<uint32_t>(std::visit(visitor_script_cell<Op::adaptor_int32>{}, refe));
				std::bitset<32> const bits{ lhs & rhs };

				eflags->m_SF = bits[31];
				eflags->m_ZF = bits.to_ulong() == 0;
				eflags->m_PF = !(bits.count() % 2);	// Flags set if count of 'true' is even number.
				eflags->m_CF = false;
				eflags->m_OF = false;
				eflags->m_AF = false;	// Technically undefined.
			};
	}

	static inline constexpr std::string_view SIG_CMP[] = { "CMP", "%num%in", "%num%in" };
	auto Parser_CMP(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& minuend = arguments[0];
		auto& subtrahend = arguments[1];

		static constexpr auto I32_MAX = std::numeric_limits<int32_t>::max();
		static constexpr auto I32_MIN = std::numeric_limits<int32_t>::min();

		// It is assumed that no ill-formed argument can reach here.
		assert(!(std::holds_alternative<std::ptrdiff_t*>(minuend)) && !(std::holds_alternative<string>(minuend)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(subtrahend)) && !(std::holds_alternative<string>(subtrahend)));

		if (std::holds_alternative<value_t>(minuend) && std::holds_alternative<value_t>(subtrahend))
		{
			auto const diff = std::get<value_t>(minuend) - std::get<value_t>(subtrahend);

			return
				[diff, eflags{ m_eflags }]() noexcept
				{
					eflags->m_SF = diff < 0;
					eflags->m_ZF = std::abs(diff) < 1e-5;
					eflags->m_PF = int32_t(diff) % 2 == 0;
					eflags->m_CF = false;	// for unsigned operation only
					eflags->m_OF = I32_MAX < diff || diff < I32_MIN;
					eflags->m_AF = false;	// No idea what to do.
				};
		}

		return
			[minuend{ std::move(minuend) }, subtrahend{ std::move(subtrahend) }, eflags{ m_eflags }]() noexcept
			{
				auto const diff =
					std::visit(visitor_script_cell{}, minuend)
					-
					std::visit(visitor_script_cell{}, subtrahend);

				eflags->m_SF = diff < 0;
				eflags->m_ZF = std::abs(diff) < 1e-5;
				eflags->m_PF = int32_t(diff) % 2 == 0;
				eflags->m_CF = false;	// for unsigned operation only
				eflags->m_OF = I32_MAX < diff || diff < I32_MIN;
				eflags->m_AF = false;	// No idea what to do.
			};
	}

	static inline constexpr std::string_view SIG_JMP[] = { "JMP", "%label" };
	auto Parser_JMP(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& label = arguments[0];

		// It is assumed that no ill-formed argument can reach here.
		assert(std::holds_alternative<std::ptrdiff_t*>(label));

		return
			[pos{ std::get<3>(std::move(label)) }, eip{ m_eip }]() noexcept
			{
				*eip = *pos;
			};
	}

	static inline constexpr std::string_view SIG_JE[] = { "JE", "%label" };
	static inline constexpr std::string_view SIG_JNE[] = { "JNE", "%label" };
	static inline constexpr std::string_view SIG_JG[] = { "JG", "%label" };
	static inline constexpr std::string_view SIG_JGE[] = { "JGE", "%label" };
	static inline constexpr std::string_view SIG_JL[] = { "JL", "%label" };
	static inline constexpr std::string_view SIG_JLE[] = { "JLE", "%label" };

	template <auto fnCondition>
	auto Parser_JMPCC(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		static_assert(requires{ { std::invoke(fnCondition, *m_eflags) } -> std::same_as<bool>; }, "Function must be able to test use with EFLAGS!");

		auto& label = arguments[0];

		// It is assumed that no ill-formed argument can reach here.
		assert(std::holds_alternative<std::ptrdiff_t*>(label));

		return
			[eflags{ m_eflags }, pos{ std::get<3>(std::move(label)) }, eip{ m_eip }]() noexcept
			{
				if (std::invoke(fnCondition, *eflags))
					*eip = *pos;
			};
	}

	static inline constexpr std::string_view SIG_LOOP[] = { "LOOP", "%label" };
	auto Parser_LOOP(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& label = arguments[0];

		// It is assumed that no ill-formed argument can reach here.
		assert(std::holds_alternative<std::ptrdiff_t*>(label));

		return
			[pos{ std::get<3>(std::move(label)) }, eip{ m_eip }, ecx{ m_ecx }]() noexcept
			{
				/*
				The loop instruction decrements ECX and jumps to the address specified by arg unless decrementing ECX caused its value to become zero.
				*/

				auto const cx = (int32_t)(*ecx) - 1;

				if (cx <= 0)
					return;

				*ecx = cx;
				*eip = *pos;
			};
	}

	static inline constexpr std::string_view SIG_LOOPE[] = { "LOOPE", "%label" };
	static inline constexpr std::string_view SIG_LOOPNE[] = { "LOOPNE", "%label" };

	template <auto fnCondition>
	auto Parser_LOOPCC(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		static_assert(requires{ { std::invoke(fnCondition, *m_eflags) } -> std::same_as<bool>; }, "Function must be able to test use with EFLAGS!");

		auto& label = arguments[0];

		// It is assumed that no ill-formed argument can reach here.
		assert(std::holds_alternative<std::ptrdiff_t*>(label));

		return
			[eflags{ m_eflags }, pos{ std::get<3>(std::move(label)) }, eip{ m_eip }, ecx{ m_ecx }]() noexcept
			{
				auto const cx = (int32_t)(*ecx) - 1;

				if (cx <= 0)
					return;

				if (std::invoke(fnCondition, *eflags))
				{
					*ecx = cx;
					*eip = *pos;
				}
			};
	}

	static inline constexpr std::string_view SIG_CALL[] = { "CALL", "%label", };
	auto Parser_CALL(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& label = arguments[0];

		// It is assumed that no ill-formed argument can reach here.
		assert(std::holds_alternative<std::ptrdiff_t*>(label));

		return
			[pos{ std::get<3>(std::move(label)) }, eip{ m_eip }, esp{ m_esp }, addr{ iSelfAddr + 1 }]() noexcept
			{
				esp->push_back(addr);	// push ret_address
				*eip = *pos;			// jmp func_label
			};
	}

	static inline constexpr std::string_view SIG_RET[] = { "RET", };
	auto Parser_RET(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		// It is assumed that no ill-formed argument can reach here.
		assert(arguments.size() == 0);

		return
			[eip{ m_eip }, esp{ m_esp }, iSelfAddr]() noexcept
			{
#ifdef _DEBUG
				assert(!esp->empty());
#else
				if (esp->empty()) [[unlikely]]
				{
					std::println("Runtime error: attempting to RET with empty stack on instruction #{}", iSelfAddr);
					std::abort();
				}
#endif
				// RET equals to:
				// pop rax
				// jmp rax

				*eip = esp->back();
				esp->pop_back();
			};
	}

	static inline constexpr std::string_view SIG_TABLE[] = { "TABLE", "%label...", };
	auto Parser_TABLE(vector<script_cell_t> arguments, string_view, std::ptrdiff_t) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto labels =
			arguments
			| std::views::transform([](auto& a) noexcept { return std::get<std::ptrdiff_t*>(std::move(a)); })
			| std::ranges::to<vector>();

		// It is assumed that no ill-formed argument can reach here.
		assert(labels.size() > 0);

		return
			[labels{ std::move(labels) }, ebx{ m_ebx }, eip{ m_eip }]() noexcept
			{
				auto const iebx = static_cast<int32_t>(*ebx);
				if (iebx < 0 || iebx >= std::ssize(labels))
					return;

				*eip = *labels[iebx];
			};
	}

	// I/O Instructions

	static inline constexpr std::string_view SIG_OUT[] = { "OUTS", "%str%in", };

	// Ownership transfered, hence parser has argument with value type of vector<>
	using parser_t = expected<instruction_t, vector<error_t>>(script_t::*)(vector<script_cell_t> arguments, string_view szLineText, std::ptrdiff_t iSelfAddr) const noexcept;
	static inline constexpr tuple<span<string_view const>, pair<uint8_t, uint8_t>, parser_t> PARSERS[] =
	{
		// Data Transfer Instructions

		{ SIG_MOV, Inspector::ArgCount(SIG_MOV).value(), &script_t::Parser_MOV },
		{ SIG_LEA, Inspector::ArgCount(SIG_LEA).value(), &script_t::Parser_MOV },	// They are the same here, with no difference between register and memory
		{ SIG_XCHG, Inspector::ArgCount(SIG_XCHG).value(), &script_t::Parser_XCHG },
		{ SIG_CMPXCHG, Inspector::ArgCount(SIG_CMPXCHG).value(), &script_t::Parser_CMPXCHG },
		{ SIG_MOVS, Inspector::ArgCount(SIG_MOVS).value(), &script_t::Parser_MOVS },
		{ SIG_CMOVE, Inspector::ArgCount(SIG_CMOVE).value(), &script_t::Parser_CMOV<&flag_register_t::Equal> },
		{ SIG_CMOVNE, Inspector::ArgCount(SIG_CMOVNE).value(), &script_t::Parser_CMOV<&flag_register_t::NotEqual> },
		{ SIG_CMOVG, Inspector::ArgCount(SIG_CMOVG).value(), &script_t::Parser_CMOV<&flag_register_t::Greater> },
		{ SIG_CMOVGE, Inspector::ArgCount(SIG_CMOVGE).value(), &script_t::Parser_CMOV<&flag_register_t::GreaterOrEq> },
		{ SIG_CMOVL, Inspector::ArgCount(SIG_CMOVL).value(), &script_t::Parser_CMOV<&flag_register_t::Lesser> },
		{ SIG_CMOVLE, Inspector::ArgCount(SIG_CMOVLE).value(), &script_t::Parser_CMOV<&flag_register_t::LesserOrEq> },

		// Control Flow Instructions

		{ SIG_TEST, Inspector::ArgCount(SIG_TEST).value(), &script_t::Parser_TEST },
		{ SIG_CMP, Inspector::ArgCount(SIG_CMP).value(), &script_t::Parser_CMP },
		{ SIG_JMP, Inspector::ArgCount(SIG_JMP).value(), &script_t::Parser_JMP },
		{ SIG_JE, Inspector::ArgCount(SIG_JE).value(), &script_t::Parser_JMPCC<&flag_register_t::Equal> },
		{ SIG_JNE, Inspector::ArgCount(SIG_JNE).value(), &script_t::Parser_JMPCC<&flag_register_t::NotEqual> },
		{ SIG_JG, Inspector::ArgCount(SIG_JG).value(), &script_t::Parser_JMPCC<&flag_register_t::Greater> },
		{ SIG_JGE, Inspector::ArgCount(SIG_JGE).value(), &script_t::Parser_JMPCC<&flag_register_t::GreaterOrEq> },
		{ SIG_JL, Inspector::ArgCount(SIG_JL).value(), &script_t::Parser_JMPCC<&flag_register_t::Lesser> },
		{ SIG_JLE, Inspector::ArgCount(SIG_JLE).value(), &script_t::Parser_JMPCC<&flag_register_t::LesserOrEq> },
		{ SIG_LOOP, Inspector::ArgCount(SIG_LOOP).value(), &script_t::Parser_LOOP },
		{ SIG_LOOPE, Inspector::ArgCount(SIG_LOOPE).value(), &script_t::Parser_LOOPCC<&flag_register_t::Equal> },
		{ SIG_LOOPNE, Inspector::ArgCount(SIG_LOOPNE).value(), &script_t::Parser_LOOPCC<&flag_register_t::NotEqual> },
		{ SIG_CALL, Inspector::ArgCount(SIG_CALL).value(), &script_t::Parser_CALL },
		{ SIG_RET, Inspector::ArgCount(SIG_RET).value(), &script_t::Parser_RET },
		{ SIG_TABLE, Inspector::ArgCount(SIG_TABLE).value(), &script_t::Parser_TABLE },

		// ENTER
		// LEAVE
		// HLT
		// NOP
		// LOCK
		// WAIT
		// PUSH
		// POP

		// I/O Instructions

		// IN
		// OUT
	};

	void Compile(std::string_view SourceText) noexcept
	{
		m_Instructions.clear();
		m_Labels.clear();
		m_Observer.clear();

		m_Observer.try_emplace("EAX", m_eax);
		m_Observer.try_emplace("EBX", m_ebx);
		m_Observer.try_emplace("ECX", m_ecx);
		m_Observer.try_emplace("EDX", m_edx);

		m_Observer.try_emplace("ESI", m_esi);
		m_Observer.try_emplace("EDI", m_edi);

		auto const rgszLines = UTIL_Split(SourceText, "\r\n");

		for (int line_num = 1; auto && szOrigLine : rgszLines)
		{
			auto const pos = szOrigLine.find_first_of(';');
			auto const szLine = UTIL_Trim(szOrigLine.substr(0, pos));
			auto const arguments = Tokenizer_Instruction(szLine);
			bool bLineHandled = false;

			auto const fnPrintError =
				[&](error_t const& err) noexcept
				{
					std::println("Compiling error: {}\n{}\n", err.m_ErrorMsg, err.ToString(8, line_num));
				};

			if (!arguments)
			{
				std::println(
					"Tokenizing error: {}\n{}\n",
					arguments.error().m_ErrorMsg, arguments.error().ToString(8, line_num)
				);

				goto LAB_NEXT;
			}

			if (arguments->empty())
				goto LAB_NEXT;

			// Is it an instruction?
			for (auto&& [signature, argc, parser] : script_t::PARSERS)
			{
				if (signature.front() != arguments->front())
					continue;

				// The address if the current line was successfully added.
				std::ptrdiff_t const iSelfAddr = std::ssize(this->m_Instructions);

				[[maybe_unused]] auto const res =

					// Transform the arguments according to signature.
					this->Parser_ProcArg(signature, argc, *arguments, szLine)

					// Call parser with processed args
					.and_then([&](auto&& args) noexcept { return std::invoke(parser, *this, std::forward<decltype(args)>(args), szLine, iSelfAddr); })

					// Insert compiled instruction
					.and_then([&](auto&& insc) noexcept { this->m_Instructions.emplace_back(std::forward<decltype(insc)>(insc)); return expected<void, vector<error_t>>{}; })

					// Print errors if any.
					.or_else([&](auto&& errs) noexcept { std::ranges::for_each(errs, fnPrintError); return expected<void, std::uint_fast8_t>{}; })
					;

				assert(res.has_value());
				bLineHandled = true;
				// Not going to break here, in case one line of source produces two instructions.
			}

			if (!bLineHandled)
			{
				error_t err{ szLine, 0 };

				// Is it a label?
				if (szLine.front() != ':' && szLine.back() == ':')
				{
					auto const ins_pos = std::ssize(this->m_Instructions);
					auto [it, bNew] = m_Labels.try_emplace(
						decltype(m_Labels)::key_type{ szLine.substr(0, szLine.size() - 1) },
						ins_pos
					);

					if (!bNew)
					{
						if (it->second >= 0)
						{
							std::println(
								"Warning: Duplicated label '{}' was ignored. Previous defined at instruction #{}\n{}\n",
								it->first, it->second, err.ToString(8, line_num)
							);
						}
						else
							// Overwrite the placeholder '-1'
							it->second = ins_pos;
					}

					goto LAB_NEXT;
				}

				std::println("Warning: Unknown instruction '{}' was ignored\n{}\n", arguments->front(), err.ToString(8, line_num));
			}

		LAB_NEXT:;
			++line_num;
		}

		for (auto&& [szName, pos] : m_Labels)
		{
			if (pos < 0)
			{
				std::println("Error: Label '{}' was referenced but nowhere to be located!\n", szName);
				Reset();
				return;
			}
		}
	}

	// Script runtime

	void Execute() noexcept
	{
		Reset();

		for (volatile auto& EIP = *m_eip; EIP < std::ssize(m_Instructions); /* Does nothing */)
		{
			auto const sav = EIP;

			try
			{
				std::invoke(m_Instructions.at(EIP));
			}
			catch (const std::exception& e)
			{
				std::println(
					"Runtime error: {}\n    Exception raised on instruction #{}",
					e.what(),
					static_cast<std::remove_cvref_t<decltype(EIP)>>(EIP)
				);
				assert(false);
			}

			if (sav == EIP)
				// Not modified, so increase it.
				++EIP;
			// Otherwise, it's been modified by things like jmp.
		}
	}

	// Only resets the state machine! Not clearing compiled instructions!
	void Reset() const noexcept
	{
		*m_eax = 0;
		*m_ebx = 0;
		*m_ecx = 0;
		*m_edx = 0;

		*m_eip = 0;

		m_esi->clear();
		m_edi->clear();

		m_eflags->Fill(false);
	}
};



static void UnitTest_Literals() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
MOV EAX, 0x100
MOV EBX, 1.048596
MOV ECX, 0o100
MOV EDX, 0b100

MOVS ESI, "Hello, world!"
MOVS EDI, ESI
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(*script.m_eax == 0x100);
	assert(*script.m_ebx == 1.048596);
	assert(*script.m_ecx == 0100);	// octal-literal in c++, fuck it
	assert(*script.m_edx == 0b100);

	assert(*script.m_esi == "Hello, world!");
	assert(*script.m_esi == *script.m_edi);
}

static void UnitTest_Expression() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
LEA EAX, [0x100 + 0x10 * 0b10 - 0o10]
LEA EBX, [EAX % 13]
LEA ECX, [e ^ pi]
LEA EDX, [ECX % 3!]
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(*script.m_eax == (0x100 + 0x10 * 0b10 - 010));
	assert(*script.m_ebx == (int32_t(*script.m_eax) % 13));
	assert(*script.m_ecx == std::pow(std::numbers::e, std::numbers::pi));
	assert(*script.m_edx == 5);	// 23 % (3*2*1)
}

static void UnitTest_Exchange() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
MOV EDX, [5! % -7]	; Unary negation is supported thanks to new tokenizer.
XCHG ECX, EDX
CMPXCHG EBX, 9.527e3	; ZF is set from this line. (EAX == EBX == 0)
CMOVE EDX, 'あ'
CMPXCHG EBX, 9.527e3	; ZF was unset from this line
CMOVE EDX, 114514
)";
	script_t script{ SOURCE };
	script.Execute();
	script.Execute();	// Test reset.

	assert(*script.m_eax == 9.527e3);
	assert(*script.m_ebx == 9.527e3);
	assert(*script.m_ecx == 1);
	assert(*script.m_edx == (double)(U'あ'));	// U+3042
}

static void UnitTest_TEST() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
TEST 0b1100, 0b0110	; == 0b0100
JNE label
MOV EAX, 1
label:
MOV EBX, 2
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(script.m_eflags->m_SF == false);
	assert(script.m_eflags->m_ZF == false);
	assert(script.m_eflags->m_PF == false);
	assert(script.m_eflags->m_CF == false);
	assert(script.m_eflags->m_OF == false);
	assert(script.m_eflags->m_AF == false);

	assert(*script.m_eax == 0);
	assert(*script.m_ebx == 2);
}

static void UnitTest_CMP() noexcept
{
	script_t script{ "CMP 1, 2" };
	script.Execute();

	assert(script.m_eflags->m_SF == true);
	assert(script.m_eflags->m_ZF == false);
	assert(script.m_eflags->m_PF == false);
	assert(script.m_eflags->m_CF == false);
	assert(script.m_eflags->m_OF == false);
	assert(script.m_eflags->m_AF == false);

	script.Compile("CMP 1, 1");
	script.Execute();

	assert(script.m_eflags->m_SF == false);
	assert(script.m_eflags->m_ZF == true);
	assert(script.m_eflags->m_PF == true);	// zero is even number.
	assert(script.m_eflags->m_CF == false);
	assert(script.m_eflags->m_OF == false);
	assert(script.m_eflags->m_AF == false);

	script.Compile("CMP 2, 1");
	script.Execute();

	assert(script.m_eflags->m_SF == false);
	assert(script.m_eflags->m_ZF == false);
	assert(script.m_eflags->m_PF == false);
	assert(script.m_eflags->m_CF == false);
	assert(script.m_eflags->m_OF == false);
	assert(script.m_eflags->m_AF == false);
}

static void UnitTest_LOOP() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
; Regular loop
MOV ECX, 5
label:
MOV EAX, [EAX + 1]
LOOP label

; Conditional loop
CMP EAX, ECX	; 5 != 1
MOV ECX, 5
label2:
MOV EBX, [EBX + 1]	; Should execute only once, i.e. EBX == 1
LOOPE label2	; Should not executed, i.e. ECX == 5
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(*script.m_eax == 5);
	assert(*script.m_ebx == 1);
	assert(*script.m_ecx == 5);
}

static void UnitTest_BuiltinFunc() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
MOV EAX, [abs(-3) + abs(3)]
MOV EBX, [round(remainder(10.3, 3.1))]	; It's absolutely 1, just rounding the floating error.
MOV ECX, [max(EAX, EBX)]
MOV EDX, [min(EAX, EBX)]
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(*script.m_eax == 6);
	assert(*script.m_ebx == 1);
	assert(*script.m_ecx == *script.m_eax);
	assert(*script.m_edx == *script.m_ebx);

	static constexpr std::string_view SOURCE2 = u8R"(
MOV	EAX, [randomf(10, 20)]
MOV	EBX, [randomi(1, 9)]
MOV	ECX, [remainder(EAX, EBX)]
MOV	EDX, [quotient(EAX, EBX)]
CMP	EAX, [EBX * EDX + ECX]
)";
	script.Compile(SOURCE2);
	script.Execute();

	assert(*script.m_eax == (*script.m_ebx * *script.m_edx + *script.m_ecx));
	assert(script.m_eflags->Equal());

	static constexpr std::string_view SOURCE3 = u8R"(
MOV	EAX, [sin ( max ( 2, 3 ) / 3 * pi )]
)";
	script.Compile(SOURCE3);
	script.Execute();

	assert(*script.m_eax == (std::sin(std::max(2.0, 3.0) / 3.0 * std::numbers::pi)));
}

static void UnitTest_CALL() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
JMP main

my_func:
	MOV EAX, [EAX + 1]
	RET

main:
	CMP	EAX, EBX	; EAX(0) == EBX(0)
	JNE if			; Not triggered
	JE	else		; Jump to 'else' label

	if:
	MOV ECX, pi		; Going to be skipped
	JMP after_branch

	else:
	MOV	ECX, phi
;	JMP after_branch; Not necessary

	after_branch:
	CALL my_func
	CMP EAX, ECX	; EAX(1) != ECX(phi)
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(*script.m_eax == 1);
	assert(*script.m_ebx == 0);
	assert(*script.m_ecx == std::numbers::phi);
	assert(*script.m_edx == 0);

	assert(script.m_eflags->NotEqual());
}

static void UnitTest_TABLE() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
MOV EBX, 2
TABLE L00, L01, L02		; Table jumps according to EBX register.
JMP end					; Should be skipped

L00:
	MOV EAX, 1
	JMP end

L01:
	MOV EAX, 2
	JMP end

L02:
	MOV EAX, 3
	JMP end

end:
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(*script.m_eax == 3);
	assert(*script.m_ebx == 2);
}

static void UnitTest_Error() noexcept
{
	std::println("=============Error Testing begins=============\n");

	static constexpr std::string_view SOURCE = u8R"(
LAB1:
;
; Ill-formed instructions
;
XCHG 1024, EDX	; Inlined comment test
XCHG EDX, Y
MOV [a + b * c - d], EIP
UNKNOWN A, B, C
LAB1:	; error here
CMP [10 @ 8], EDX	; unknown operator@
WTF a @b c
MOV EAX, [atan(3)]
MOV EAX, [arctan(3)]
MOVS EDI, [strlen(ESI)]
TABLE
)";
	script_t script{};
	script.Compile(SOURCE);
	script.Execute();

	assert(*script.m_eax == 0);
	assert(*script.m_ebx == 0);
	assert(*script.m_ecx == 0);
	assert(*script.m_edx == 0);

	std::println("=============Error Testing ends=============\n");
}

#ifdef _MSC_VER
int wmain(int argc, wchar_t* argv[], wchar_t* envp[]) noexcept
#else
int main(int, char*[]) noexcept
#endif
{
	UnitTest_Literals();
	UnitTest_Expression();
	UnitTest_Exchange();
	UnitTest_TEST();
	UnitTest_CMP();
	UnitTest_LOOP();
	UnitTest_BuiltinFunc();
	UnitTest_CALL();
	UnitTest_TABLE();
	UnitTest_Error();
}
