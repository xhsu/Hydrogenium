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
using std::variant;
using std::vector;

inline constexpr std::string_view SCRIPT = u8R"(
MOV EAX, 0x100
MOV EBX, EAX
MOV ECX, 0o100
MOV EDX, 0b100
XCHG ECX, EDX	; Inlined comment test #1
CMPXCHG EBX, 100.0
;
; Ill-formed instructions
;
XCHG 1024, EDX	; Inlined comment test #2
XCHG EDX, Y
MOV [a + b * c - d], BULLSHIT
UNKNOWN A, B, C
)";

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
		constexpr char separator = ',';
		constexpr string_view spaces = " \t\f\v";

		m_Underscore.reserve(8);

		if (m_Text.empty())
			return;

		auto const spc_pos = m_Text.find_first_of(spaces);
		if (spc_pos == m_Text.npos)
		{
			m_SegmentsUnderline.emplace_back(m_Underscore.data(), m_Underscore.size());
			return;
		}

		auto const opr_pos = m_Text.find_first_not_of(spaces, spc_pos);
		assert(opr_pos < m_Text.length());	// 's' had been trimmed, hence there must be something after spaces.

		m_SegmentsText.emplace_back(m_Text.substr(0, spc_pos));
		m_SegmentsUnderline.emplace_back(m_Underscore.data(), std::min(m_Underscore.size(), spc_pos));

		for (auto lastPos = m_Text.find_first_not_of(separator, opr_pos), pos = m_Text.find_first_of(separator, lastPos);
			m_Text.npos != pos || m_Text.npos != lastPos;
			lastPos = m_Text.find_first_not_of(separator, pos), pos = m_Text.find_first_of(separator, lastPos)
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
};

struct script_t final
{
	// Script registers

	using instruction_t = move_only_function<void() const noexcept>;
	using value_t = double;
	using expr_t = move_only_function<value_t() const noexcept>;
	using valref_t = shared_ptr<value_t>;

	vector<instruction_t> m_Instructions{};
	valref_t m_eax{ std::make_shared<value_t>() };
	valref_t m_ebx{ std::make_shared<value_t>() };
	valref_t m_ecx{ std::make_shared<value_t>() };
	valref_t m_edx{ std::make_shared<value_t>() };

	// shared_ptr<uint16_t> m_eip{ std::make_shared<uint16_t>() };
	// shared_ptr<uint32_t> m_esp{ std::make_shared<uint32_t>() };
	// shared_ptr<uint32_t> m_ebp{ std::make_shared<uint32_t>() };

	// ESI
	// EDI

	shared_ptr<flag_register_t> m_eflags{ std::make_shared<flag_register_t>() };

	// Script parser

	auto Parser_GetRegister(string_view name) const noexcept -> valref_t
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

	auto Parser_GetDest(string_view argument) const noexcept -> expected<valref_t, string_view>
	{
		if (argument == "EAX")
			return m_eax;
		if (argument == "EBX")
			return m_ebx;
		if (argument == "ECX")
			return m_ecx;
		if (argument == "EDX")
			return m_edx;

		return std::unexpected("Not a valid storage or dest");
	}

	auto Parser_GetSrc(string_view argument) const noexcept -> expected<variant<valref_t, value_t>, string_view>
	{
		if (argument == "EAX")
			return m_eax;
		if (argument == "EBX")
			return m_ebx;
		if (argument == "ECX")
			return m_ecx;
		if (argument == "EDX")
			return m_edx;

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

			return std::unexpected("Invalid immediate integer number");
		}

		value_t ret{};
		if (std::from_chars(argument.data(), argument.data() + argument.size(), ret).ec == std::errc{})
			return ret;

		return std::unexpected("Invalid immediate floating-point number");
	}

	auto Parser_ProcSig(span<string_view const> parameters, span<string_view const> arguments, string_view szLineText) const noexcept -> expected<vector<variant<valref_t, value_t, expr_t>>, vector<error_t>>
	{
		vector<variant<valref_t, value_t, expr_t>> processed{};
		vector<error_t> errors{};

		if (arguments.size() != parameters.size())
		{
			errors.emplace_back(szLineText, 0, std::format("Expected {} arguments but {} received", parameters.size(), arguments.size()));
			return std::unexpected(std::move(errors));
		}

		for (int i = 1; i < std::ssize(arguments); ++i)
		{
			if (parameters[i] == "%in")
			{
				if (auto res = Parser_GetSrc(arguments[i]); res.has_value())
				{
					std::visit(
						[&](auto&& a) noexcept { processed.emplace_back(std::forward<decltype(a)>(a)); },
						std::move(res).value()
					);
				}
				else
					// Pass down the reason why this arg cannot be processed.
					errors.emplace_back(szLineText, i, std::format("Ill-formed arg #{}: {}", i, std::move(res).error()));
			}
			else if (parameters[i] == "%out" || parameters[i] == "%inout")
			{
				if (auto res = Parser_GetDest(arguments[i]); res.has_value())
					processed.emplace_back(std::move(res).value());
				else
					errors.emplace_back(szLineText, i, std::format("Ill-formed arg #{}: {}", i, std::move(res).error()));
			}
			else
				errors.emplace_back(szLineText, i, std::format("Unrecognized param '{}' found in signature", parameters[i]));
		}

		if (!errors.empty())
			return std::unexpected(std::move(errors));

		return processed;
	}

	static inline constexpr std::string_view SIG_MOV[] = { "MOV", "%out", "%in" };
	auto Parser_MOV(span<string_view const> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto res = Parser_ProcSig(SIG_MOV, arguments, szLineText);
		if (!res)
			return std::unexpected(std::move(res).error());

		auto processed{ std::move(res).value() };
		auto& parsed_dest = processed[0];
		auto& parsed_src = processed[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_dest)));
		assert((std::holds_alternative<valref_t>(parsed_src) || std::holds_alternative<value_t>(parsed_src)));

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

		default:
			std::unreachable();
			break;
		}
	}

	static inline constexpr std::string_view SIG_XCHG[] = { "XCHG", "%inout", "%inout" };
	auto Parser_XCHG(span<string_view const> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto res = Parser_ProcSig(SIG_XCHG, arguments, szLineText);
		if (!res)
			return std::unexpected(std::move(res).error());

		auto processed{ std::move(res).value() };
		auto& parsed_lhs = processed[0];
		auto& parsed_rhs = processed[1];

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
	auto Parser_CMPXCHG(span<string_view const> arguments, string_view szLineText) const noexcept -> expected<instruction_t, vector<error_t>>
	{
		auto res = Parser_ProcSig(SIG_CMPXCHG, arguments, szLineText);
		if (!res)
			return std::unexpected(std::move(res).error());

		auto processed{ std::move(res).value() };
		auto& parsed_arg1 = processed[0];
		auto& parsed_arg2 = processed[1];

		// It is assumed that no ill-formed argument can reach here.
		assert((std::holds_alternative<valref_t>(parsed_arg1)));
		assert((std::holds_alternative<valref_t>(parsed_arg2) || std::holds_alternative<value_t>(parsed_arg2)));

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
					//eflags->m_CF = diff < 0;	// For unsigned situation
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
					//eflags->m_CF = diff < 0;	// For unsigned situation
					eflags->m_OF = I32_MAX < diff || diff < I32_MIN;
					eflags->m_PF = int32_t(diff) % 2 == 0;
					eflags->m_AF = false;	// LUNA: I have no idea how to set the AF.

					if (eflags->m_ZF)
						*arg1 = arg2;
					else
						*eax = *arg1;
				};

		default:
			std::unreachable();
		}
	}

	using parser_t = decltype(&script_t::Parser_MOV);
	static inline constexpr std::pair<span<string_view const>, parser_t> PARSERS[] =
	{
		{ SIG_MOV, &script_t::Parser_MOV },
		{ SIG_XCHG, &script_t::Parser_XCHG },
		{ SIG_CMPXCHG, &script_t::Parser_CMPXCHG },
	};

	// Script runtime

	void Execute() noexcept
	{
		for (auto&& fn : m_Instructions)
			std::invoke(fn);
	}

	void Print() const noexcept
	{
		std::println("EAX: {}", *m_eax);
		std::println("EBX: {}", *m_ebx);
		std::println("ECX: {}", *m_ecx);
		std::println("EDX: {}", *m_edx);
	}
};

int main() noexcept
{
	script_t script{};
	auto const rgszLines = UTIL_Split(SCRIPT, "\r\n");

	for (int line_num = 1; auto&& szOrigLine : rgszLines)
	{
		auto const pos = szOrigLine.find_first_of(';');
		auto const szLine = szOrigLine.substr(0, pos);
		auto const arguments = PARSER_Instruction(szLine);
		bool bLineHandled = false;

		if (arguments.empty())
			goto LAB_NEXT;

		for (auto&& [signature, parser] : script_t::PARSERS)
		{
			if (signature.front() != arguments.front())
				continue;

			auto res = std::invoke(parser, script, arguments, szLine);
			if (res)
				script.m_Instructions.emplace_back(std::move(res).value());
			else
			{
				for (auto&& err : res.error())
					std::println("Compiling error: {}\n{}\n", err.m_ErrorMsg, err.ToString(8, line_num));
			}

			bLineHandled = true;
			// Not going to break here, in case one line of source produces two instructions.
		}

		if (!bLineHandled)
		{
			error_t err{ szLine, 0 };
			std::println("Warning: Unknown instruction '{}' was ignored\n{}\n", arguments.front(), err.ToString(8, line_num));
		}

	LAB_NEXT:;
		++line_num;
	}

	script.Execute();
	script.Print();
}
