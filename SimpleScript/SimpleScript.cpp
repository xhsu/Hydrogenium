// SimpleScript.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#ifdef __INTELLISENSE__
#define _MSVC_TESTING_NVCC
#include <__msvc_all_public_headers.hpp>
#undef _MSVC_TESTING_NVCC
#else
#include <cassert>
import std.compat;
#endif

using std::expected;
using std::move_only_function;
using std::optional;
using std::shared_ptr;
using std::span;
using std::string;
using std::string_view;
using std::unordered_map;
using std::variant;
using std::vector;

using namespace std::literals;

template <size_t Length>
struct fixed_string
{
	constexpr fixed_string(const char(&arr)[Length]) noexcept { std::ranges::copy(arr, m_arr); }

	constexpr operator std::string_view() const noexcept { return m_arr; }

	char m_arr[Length + 1] = {}; // +1 for null terminator
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

static constexpr auto PARSER_Instruction(std::string_view s) noexcept -> std::vector<std::string_view>
{
	constexpr std::string_view DELIM = " \t\f\v";

	s = UTIL_Trim(s);
	if (s.empty())
		return {};

	auto const spc_pos = s.find_first_of(DELIM);
	if (spc_pos == s.npos)
		return { s };

	auto const opr_pos = s.find_first_not_of(DELIM, spc_pos);
	assert(opr_pos < s.length());	// 's' had been trimmed, hence there must be something after spaces.

	std::vector<std::string_view> ret{ s.substr(0, spc_pos) };
	ret.append_range(
		UTIL_Split(s.substr(opr_pos), ",")
		| std::views::transform(&UTIL_Trim)
	);

	return ret;
}

using instruction_t = move_only_function<void() const noexcept>;	// Representing an single action
using value_t = double;
using expr_t = move_only_function<value_t() const noexcept>;
using valref_t = shared_ptr<value_t>;
using script_cell_t = variant<valref_t, value_t, expr_t, std::ptrdiff_t*>;

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
		else if constexpr (requires { { std::invoke(a) } -> std::convertible_to<value_t>; })
		{
			return std::invoke(m_proj, std::invoke(a));
		}
		else
		{
			return std::invoke(m_proj, a);
		}
	}
};

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

		constexpr expected<expr_t, value_t> operator()(value_t lhs, value_t rhs) const noexcept
		{
			return std::unexpected(std::invoke(m_op, m_proj(lhs), m_proj(rhs)));
		}

		constexpr expected<expr_t, value_t> operator()(value_t operand) const noexcept
		{
			return std::unexpected(std::invoke(m_op, m_proj(operand)));
		}

		expected<expr_t, value_t> operator()(auto lhs, auto rhs) const noexcept
		{
			return
				[lhs{ std::move(lhs) }, rhs{ std::move(rhs) }]() noexcept -> value_t
				{
					return std::invoke(
						m_op,
						m_visitor(lhs), m_visitor(rhs)
					);
				};
		}

		expected<expr_t, value_t> operator()(auto operand) const noexcept
		{
			return
				[operand{ std::move(operand) }]() noexcept -> value_t
				{
					return std::invoke(
						m_op,
						m_visitor(operand)
					);
				};
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

					return (value_t)factorial((int32_t)visitor_script_cell{}(arg));
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
	if (bBin && bindig_count == (s.size() - 1))
		return true;
	if (bOct && octdig_count == (s.size() - 1))
		return true;
	if (decdig_count == s.size())
		return true;
	if (bHex && hexdig_count == (s.size() - 1))
		return true;

	// Floating point literal.
	if ((e_count == 1 || dot_count == 1) && decdig_count == (s.size() - dot_count - e_count - sign_count) && bIsBackDigit)
		return true;	// floating point number must not be hex.

	return false;
}

namespace Op
{
	enum struct EAssoc : uint8_t
	{
		Undefined = 0xFF,
		Left = 1,
		Right = 0,
	};

	template <fixed_string ID, EAssoc LEFT_ASSOC = EAssoc::Undefined, int_fast8_t PRECED = 0, uint_fast8_t ARG_COUNT = 0, typename FN = std::identity>
	struct script_operator_t final
	{
		static inline constexpr std::string_view m_id{ ID };
		static inline constexpr auto m_left_assoc = LEFT_ASSOC;
		static inline constexpr auto m_preced = PRECED;
		static inline constexpr auto m_arg_count = ARG_COUNT;
		static inline constexpr auto functor = FN{};
	};

	using factorial_t = script_operator_t<"!", EAssoc::Left, 5, 1, functor_factorial>;

	using power_t = script_operator_t<"^", EAssoc::Right, 4, 2, functor_power>;

	using multiply_t = script_operator_t<"*", EAssoc::Left, 3, 2, functor<std::multiplies<>>>;
	using divide_t = script_operator_t<"/", EAssoc::Left, 3, 2, functor<std::divides<>>>;
	using modulo_t = script_operator_t<"%", EAssoc::Left, 3, 2, functor<std::modulus<>, adaptor_int32>>;	// Only int can take remainder.

	using plus_t = script_operator_t<"+", EAssoc::Left, 2, 2, functor<std::plus<>>>;
	using minus_t = script_operator_t<"-", EAssoc::Left, 2, 2, functor<std::minus<>>>;

	using assign_t = script_operator_t<"=", EAssoc::Right, 1, 2, functor_dummy>;	// Not available now.

	constexpr auto impl_all_op_wrapper(auto&& impl) noexcept
	{
		return impl.template operator() <
			Op::factorial_t,
			Op::power_t,
			Op::multiply_t, Op::divide_t, Op::modulo_t,
			Op::plus_t, Op::minus_t,
			Op::assign_t
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
					= (... || ((Tys::m_id == s) && (void(ret = Tys::m_left_assoc), true)));

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

	auto const impl =
		[]<typename... Tys>(std::string_view const& s) noexcept
	{
		return (... || (Tys::m_id == s));
	};

	return impl.template operator()<
		Op::factorial_t,
		Op::power_t,
		Op::multiply_t, Op::divide_t, Op::modulo_t,
		Op::plus_t, Op::minus_t,
		Op::assign_t
	>(s);
}

static_assert(IsLiteral("1234") && IsLiteral("1e8"));
static_assert(IsLiteral("0xABCD"));
static_assert(!IsLiteral("0o5678"));	// Bad: oct number containing '8'
static_assert(!IsLiteral("0.1.1"));	// Bad: Version number.
static_assert(IsLiteral("-12.34e-5"));
static_assert(!IsLiteral("--12.34e-5"));	// Bad: too many signs
static_assert(!IsLiteral("-12.34ef5"));	// Bad: floating with 'f'
static_assert(!IsLiteral("1.") && !IsLiteral("1e"));	// Bad: Bad fp format.

constexpr auto Tokenizer(string_view s) noexcept -> expected<vector<string_view>, string_view>
{
	// 1. Parse the string as long as possible, like pre-c++11
	// 2. Kicks off the last character then check again.

	expected<vector<string_view>, string_view> ret{ std::in_place };
	ret->reserve(s.size());

	bool bAllowSignOnNext = true;	// Should not being reset inter-tokens
	for (size_t pos = 0; pos < s.size(); /* Does nothing */)
	{
		auto len = s.size() - pos;
		while (len > 0)
		{
			auto const token = s.substr(pos, len);
			auto const bIsIdentifier = IsIdentifier(token);
			auto const bIsLiteral = IsLiteral(token, bAllowSignOnNext);
			auto const bIsOperator = IsOperator(token);

			if (bIsIdentifier || bIsLiteral || bIsOperator)
			{
				ret->emplace_back(token);
				bAllowSignOnNext = bIsOperator;	// If it is an operator prev, then a sign is allow. Things like: x ^ -2 (x to the power of neg 2)
				break;
			}
			else if (len == 1 && std::isspace(s[pos]))
				break;	// space gets skipped without considered as token.

			--len;
		}

		if (!len)
			// The segment was problematically.
			//return std::unexpected(std::format("Segment '{}' at pos {} cannot be tokenized.", s.substr(pos), pos));
			return std::unexpected("Segment cannot be tokenized.");
		else
			// If parsed, something must be inserted.
			pos += len;
	}

	return ret;
}

constexpr auto ShuntingYardAlgorithm(string_view s) noexcept -> expected<vector<string_view>, string_view>
{
	vector<string_view> ret{};
	vector<string_view> op_stack{};

	auto identifiers = Tokenizer(s);
	if (!identifiers)
		return std::unexpected(std::move(identifiers).error());

	for (auto&& token : identifiers.value())
	{
		// Is number?
		if (IsLiteral(token) || IsIdentifier(token))
		{
			ret.push_back(token);
		}

		// Is a function?
		// push it onto the operator stack
		// LUNA: we don't have this feature, so skip.

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

				/* LUNA: we don't have this feature.
				if there is a function token at the top of the operator stack, then:
					pop the function from the operator stack into the output queue
				*/
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

constexpr bool SYA_Test() noexcept
{
	auto const res = ShuntingYardAlgorithm("3+4*2/(1-5)^2^3");
	auto const rpn =
		*res
		| std::views::join
		| std::ranges::to<string>();

	return rpn == "342*15-23^^/+";
}
static_assert(SYA_Test());



struct error_t final
{
	constexpr error_t(error_t const&) noexcept = default;
	constexpr error_t(error_t&&) noexcept = default;
	constexpr ~error_t() noexcept = default;

	constexpr error_t& operator=(error_t const&) noexcept = default;
	constexpr error_t& operator=(error_t&&) noexcept = default;

	constexpr error_t(string_view str, string_view sprtr_ins = " \t\f\v", string_view sprtr_oprd = ",") noexcept
		: m_Text{ UTIL_Trim(str) }, m_Underscore(m_Text.size(), ' ')
	{
		m_Underscore.reserve(8);

		if (m_Text.empty())
			return;

		auto const spc_pos = m_Text.find_first_of(sprtr_ins);
		if (spc_pos == m_Text.npos)
		{
			m_SegmentsUnderline.emplace_back(m_Underscore.data(), m_Underscore.size());
			return;
		}

		auto const opr_pos = m_Text.find_first_not_of(sprtr_ins, spc_pos);
		assert(opr_pos < m_Text.length());	// 's' had been trimmed, hence there must be something after spaces.

		m_SegmentsText.emplace_back(m_Text.substr(0, spc_pos));
		m_SegmentsUnderline.emplace_back(m_Underscore.data(), std::min(m_Underscore.size(), spc_pos));

		for (auto lastPos = m_Text.find_first_not_of(sprtr_oprd, opr_pos), pos = m_Text.find_first_of(sprtr_oprd, lastPos);
			m_Text.npos != pos || m_Text.npos != lastPos;
			lastPos = m_Text.find_first_not_of(sprtr_oprd, pos), pos = m_Text.find_first_of(sprtr_oprd, lastPos)
			)
		{
			//auto const seg = UTIL_Trim(string_view{ &m_Underscore[lastPos], std::min(pos - lastPos, m_Underscore.size() - lastPos) });
			auto const TextSeg =
				UTIL_Trim({ &m_Text[lastPos], (pos == m_Text.npos ? (&m_Text.back() + 1) : &m_Text[pos]) });
			m_SegmentsText.emplace_back(TextSeg);

			auto const underline_begin = TextSeg.data() - m_Text.data();
			auto const underline_end = TextSeg.data() + TextSeg.length() - m_Text.data();
			assert(underline_begin < underline_end);	// Must not be equal. Equal here means empty range.

			m_SegmentsUnderline.emplace_back(
				&m_Underscore[underline_begin],
				TextSeg.length()
			);
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
	valref_t m_eax{ std::make_shared<value_t>() };
	valref_t m_ebx{ std::make_shared<value_t>() };
	valref_t m_ecx{ std::make_shared<value_t>() };
	valref_t m_edx{ std::make_shared<value_t>() };

	shared_ptr<std::ptrdiff_t> m_eip{ std::make_shared<std::ptrdiff_t>() };
	// shared_ptr<uint32_t> m_esp{ std::make_shared<uint32_t>() };
	// shared_ptr<uint32_t> m_ebp{ std::make_shared<uint32_t>() };

	// ESI
	// EDI

	shared_ptr<flag_register_t> m_eflags{ std::make_shared<flag_register_t>() };
	unordered_map<string, std::ptrdiff_t, std::hash<string_view>, std::equal_to<>> m_Labels{};

	// Script parser

	__forceinline auto Parser_GetRegister(string_view name) const noexcept -> valref_t
	{
		if (name == "EAX")
			return m_eax;
		if (name == "EBX")
			return m_ebx;
		if (name == "ECX")
			return m_ecx;
		if (name == "EDX")
			return m_edx;

		return nullptr;
	}

	__forceinline static auto Parser_GetImmediate(string_view argument) noexcept -> value_t
	{
		if (argument == "e")
			return std::numbers::e;
		if (argument == "pi")
			return std::numbers::pi;
		if (argument == "phi")
			return std::numbers::phi;

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

	auto Parser_GetOutput(string_view argument) const noexcept -> expected<valref_t, string_view>
	{
		if (argument == "EIP" || argument == "IP")
			return std::unexpected("Instruction Pointer must not be an output destination");

		if (auto reg = Parser_GetRegister(argument))
			return reg;

		return std::unexpected("Not a valid storage or dest");
	}

	auto Parser_GetInput(string_view argument) const noexcept -> expected<variant<valref_t, value_t, expr_t>, string_view>
	{
		if (argument == "EIP" || argument == "IP")
			return std::unexpected("Instruction Pointer ought not to be read");

		if (argument.length() && argument.front() == '[' && argument.back() == ']')
		{
			argument = argument.substr(1, argument.size() - 2);
			auto const PostfixNotation = ShuntingYardAlgorithm(argument);

			if (!PostfixNotation)
				return std::unexpected(PostfixNotation.error());

			// eval postfix notation expr
			vector<variant<valref_t, value_t, expr_t>> num_stack{};

			for (auto&& token : PostfixNotation.value())
			{
				if (IsIdentifier(token) || IsLiteral(token))
				{
					auto parsed_input = Parser_GetInput(token);
					if (!parsed_input)
						return std::unexpected(parsed_input.error());

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
				else
					return std::unexpected("Unrecognized token found in expression");
			}

			if (num_stack.empty())
				return std::unexpected("Bad expression");

			// For invoking move constructor of variant<>
			return std::move(num_stack.front());
		}

		if (auto reg = Parser_GetRegister(argument); reg != nullptr)	// nullptr here is no found
			return reg;

		if (auto const val = Parser_GetImmediate(argument); val == val)	// NaN here is no found
			return val;

		return std::unexpected("Not a register, immediate number or expression");
	}

	auto Parser_ProcSig(span<string_view const> parameters, span<string_view const> arguments, string_view szLineText) noexcept -> expected<vector<script_cell_t>, vector<error_t>>
	{
		vector<script_cell_t> processed{};
		vector<error_t> errors{};

		if (arguments.size() != parameters.size())
		{
			errors.emplace_back(szLineText, 0, std::format("Expected {} operand but {} received", parameters.size(), arguments.size()));
			return std::unexpected(std::move(errors));
		}

		for (int i = 1; i < std::ssize(arguments); ++i)
		{
			if (parameters[i] == "%in")
			{
				if (auto res = Parser_GetInput(arguments[i]); res.has_value())
				{
					std::visit(
						[&](auto&& a) noexcept { processed.emplace_back(std::forward<decltype(a)>(a)); },
						std::move(res).value()
					);
				}
				else
					// Pass down the reason why this arg cannot be processed.
					errors.emplace_back(szLineText, i, std::format("Ill-formed operand #{}: {}", i, std::move(res).error()));
			}
			else if (parameters[i] == "%out" || parameters[i] == "%inout")
			{
				if (auto res = Parser_GetOutput(arguments[i]); res.has_value())
					processed.emplace_back(std::move(res).value());
				else
					errors.emplace_back(szLineText, i, std::format("Ill-formed operand #{}: {}", i, std::move(res).error()));
			}
			else if (parameters[i] == "%label")
			{
				// Creating the label here as well, in case it's forward referenced.
				auto [it, bNew] = m_Labels.try_emplace(decltype(m_Labels)::key_type{ arguments[i] }, -1);
				processed.emplace_back(std::addressof(it->second));
			}
			else
				errors.emplace_back(szLineText, i, std::format("Unrecognized param '{}' found in signature", parameters[i]));
		}

		if (!errors.empty())
			return std::unexpected(std::move(errors));

		return processed;
	}

	// Data Transfer Instructions

	static inline constexpr std::string_view SIG_MOV[] = { "MOV", "%out", "%in" };
	static inline constexpr std::string_view SIG_LEA[] = { "LEA", "%out", "%in" };
	auto Parser_MOV(vector<script_cell_t> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_dest = arguments[0];
		auto& parsed_src = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_dest)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(parsed_src)));

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

	static inline constexpr std::string_view SIG_XCHG[] = { "XCHG", "%inout", "%inout" };
	auto Parser_XCHG(vector<script_cell_t> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
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

	static inline constexpr std::string_view SIG_CMPXCHG[] = { "CMPXCHG", "%inout", "%in" };
	auto Parser_CMPXCHG(vector<script_cell_t> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_arg1 = arguments[0];
		auto& parsed_arg2 = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_arg1)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(parsed_arg2)));

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

	static inline constexpr std::string_view SIG_CMOVE[] = { "CMOVE", "%out", "%in" };
	static inline constexpr std::string_view SIG_CMOVNE[] = { "CMOVNE", "%out", "%in" };
	static inline constexpr std::string_view SIG_CMOVG[] = { "CMOVG", "%out", "%in" };
	static inline constexpr std::string_view SIG_CMOVGE[] = { "CMOVGE", "%out", "%in" };
	static inline constexpr std::string_view SIG_CMOVL[] = { "CMOVL", "%out", "%in" };
	static inline constexpr std::string_view SIG_CMOVLE[] = { "CMOVLE", "%out", "%in" };

	template <auto fnCondition>
	auto Parser_CMOV(vector<script_cell_t> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& parsed_dest = arguments[0];
		auto& parsed_src = arguments[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_dest)));
		assert(!(std::holds_alternative<std::ptrdiff_t*>(parsed_src)));

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

	static inline constexpr std::string_view SIG_TEST[] = { "TEST", "%in", "%in" };
	auto Parser_TEST(vector<script_cell_t> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& accumulator = arguments[0];
		auto& reference = arguments[1];

		// It is assumed that no ill-formed argument can reach here.

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
				auto const lhs = std::bit_cast<uint32_t>((int32_t)std::visit(visitor_script_cell{}, accu));
				auto const rhs = std::bit_cast<uint32_t>((int32_t)std::visit(visitor_script_cell{}, refe));
				std::bitset<32> const bits{ lhs & rhs };

				eflags->m_SF = bits[31];
				eflags->m_ZF = bits.to_ulong() == 0;
				eflags->m_PF = !(bits.count() % 2);	// Flags set if count of 'true' is even number.
				eflags->m_CF = false;
				eflags->m_OF = false;
				eflags->m_AF = false;	// Technically undefined.
			};
	}

	static inline constexpr std::string_view SIG_CMP[] = { "CMP", "%in", "%in" };
	auto Parser_CMP(vector<script_cell_t> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto& minuend = arguments[0];
		auto& subtrahend = arguments[1];

		static constexpr auto I32_MAX = std::numeric_limits<int32_t>::max();
		static constexpr auto I32_MIN = std::numeric_limits<int32_t>::min();

		// It is assumed that no ill-formed argument can reach here.

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

	static inline constexpr std::string_view SIG_JE[] = { "JE", "%label" };
	static inline constexpr std::string_view SIG_JNE[] = { "JNE", "%label" };
	static inline constexpr std::string_view SIG_JG[] = { "JG", "%label" };
	static inline constexpr std::string_view SIG_JGE[] = { "JGE", "%label" };
	static inline constexpr std::string_view SIG_JL[] = { "JL", "%label" };
	static inline constexpr std::string_view SIG_JLE[] = { "JLE", "%label" };

	template <auto fnCondition>
	auto Parser_JMP(vector<script_cell_t> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
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

	// Ownership transfered, hence parser has argument with value type of vector<>
	using parser_t = decltype(&script_t::Parser_MOV);
	static inline constexpr std::pair<span<string_view const>, parser_t> PARSERS[] =
	{
		// Data Transfer Instructions

		{ SIG_MOV, &script_t::Parser_MOV },
		{ SIG_LEA, &script_t::Parser_MOV },	// They are the same here, with no difference between register and memory
		{ SIG_XCHG, &script_t::Parser_XCHG },
		{ SIG_CMPXCHG, &script_t::Parser_CMPXCHG },
		{ SIG_CMOVE, &script_t::Parser_CMOV<&flag_register_t::Equal> },
		{ SIG_CMOVNE, &script_t::Parser_CMOV<&flag_register_t::NotEqual> },
		{ SIG_CMOVG, &script_t::Parser_CMOV<&flag_register_t::Greater> },
		{ SIG_CMOVGE, &script_t::Parser_CMOV<&flag_register_t::GreaterOrEq> },
		{ SIG_CMOVL, &script_t::Parser_CMOV<&flag_register_t::Lesser> },
		{ SIG_CMOVLE, &script_t::Parser_CMOV<&flag_register_t::LesserOrEq> },

		// Control Flow Instructions

		{ SIG_TEST, &script_t::Parser_TEST },
		{ SIG_CMP, &script_t::Parser_CMP },
		{ SIG_JE, &script_t::Parser_JMP<&flag_register_t::Equal> },
		{ SIG_JNE, &script_t::Parser_JMP<&flag_register_t::NotEqual> },
		{ SIG_JG, &script_t::Parser_JMP<&flag_register_t::Greater> },
		{ SIG_JGE, &script_t::Parser_JMP<&flag_register_t::GreaterOrEq> },
		{ SIG_JL, &script_t::Parser_JMP<&flag_register_t::Lesser> },
		{ SIG_JLE, &script_t::Parser_JMP<&flag_register_t::LesserOrEq> },
	};

	void Compile(std::string_view SourceText) noexcept
	{
		m_Instructions.clear();
		m_Labels.clear();

		auto const rgszLines = UTIL_Split(SourceText, "\r\n");

		for (int line_num = 1; auto && szOrigLine : rgszLines)
		{
			auto const pos = szOrigLine.find_first_of(';');
			auto const szLine = UTIL_Trim(szOrigLine.substr(0, pos));
			auto const arguments = PARSER_Instruction(szLine);
			bool bLineHandled = false;

			auto const fnPrintError =
				[&](error_t const& err) noexcept
				{
					std::println("Compiling error: {}\n{}\n", err.m_ErrorMsg, err.ToString(8, line_num));
				};

			if (arguments.empty())
				goto LAB_NEXT;

			for (auto&& [signature, parser] : script_t::PARSERS)
			{
				if (signature.front() != arguments.front())
					continue;

				[[maybe_unused]] auto const res =

					// Transform the arguments according to signature.
					this->Parser_ProcSig(signature, arguments, szLine)

					// Call parser with processed args
					.and_then([&](auto&& args) noexcept { return std::invoke(parser, *this, std::forward<decltype(args)>(args), szLine); })

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

				std::println("Warning: Unknown instruction '{}' was ignored\n{}\n", arguments.front(), err.ToString(8, line_num));
			}

		LAB_NEXT:;
			++line_num;
		}

		for (auto&& [szName, pos] : m_Labels)
		{
			if (pos < 0)
				std::println("Error: Label '{}' was referenced but nowhere to be located!\n", szName);
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
			}

			if (sav == EIP)
				// Not modified, so increase it.
				++EIP;
			// Otherwise, it's been modified by things like jmp.
		}
	}

	void Reset() const noexcept
	{
		*m_eax = 0;
		*m_ebx = 0;
		*m_ecx = 0;
		*m_edx = 0;

		*m_eip = 0;

		m_eflags->Fill(false);
	}

	void Print() const noexcept
	{
		std::println("EAX: {}", *m_eax);
		std::println("EBX: {}", *m_ebx);
		std::println("ECX: {}", *m_ecx);
		std::println("EDX: {}", *m_edx);
	}
};


static void UnitTest_Literals() noexcept
{
	static constexpr std::string_view SOURCE = u8R"(
MOV EAX, 0x100
MOV EBX, 1.048596
MOV ECX, 0o100
MOV EDX, 0b100
)";
	script_t script{ SOURCE };
	script.Execute();

	assert(*script.m_eax == 0x100);
	assert(*script.m_ebx == 1.048596);
	assert(*script.m_ecx == 0100);	// octal-literal in c++, fuck it
	assert(*script.m_edx == 0b100);
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
CMOVE EDX, 42
CMPXCHG EBX, 9.527e3	; ZF was unset from this line
CMOVE EDX, 114514
)";
	script_t script{ SOURCE };
	script.Execute();
	script.Execute();	// Test reset.

	assert(*script.m_eax == 9.527e3);
	assert(*script.m_ebx == 9.527e3);
	assert(*script.m_ecx == 1);
	assert(*script.m_edx == 42);
}

static void UnitTest_Error() noexcept
{
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
)";
	script_t script{};
	script.Compile(SOURCE);
	script.Execute();

	assert(*script.m_eax == 0);
	assert(*script.m_ebx == 0);
	assert(*script.m_ecx == 0);
	assert(*script.m_edx == 0);
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

int main() noexcept
{
	UnitTest_Literals();
	UnitTest_Expression();
	UnitTest_Exchange();
	UnitTest_Error();
	UnitTest_TEST();
	UnitTest_CMP();
}
