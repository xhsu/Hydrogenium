// SimpleScript.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#ifdef __INTELLISENSE__
#define _MSVC_TESTING_NVCC
#include <__msvc_all_public_headers.hpp>
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
MOV EAX 0x100
MOV EBX EAX
MOV ECX 0o100
MOV EDX 0b100
XCHG ECX EDX	; Inlined comment test #1
;
; Ill-formed instructions
;
XCHG 1024 EDX	; Inlined comment test #2
XCHG EDX Y
MOV 1024 BULLSHIT
UNKNOWN A B C
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

struct error_t final
{
	constexpr error_t(error_t const&) noexcept = default;
	constexpr error_t(error_t&&) noexcept = default;
	constexpr ~error_t() noexcept = default;

	constexpr error_t& operator=(error_t const&) noexcept = default;
	constexpr error_t& operator=(error_t&&) noexcept = default;

	constexpr error_t(string_view str, string_view delim = " \t\f\v") noexcept
		: m_Text{ std::move(str) }, m_Underscore(m_Text.size(), ' ')
	{
		m_Underscore.reserve(8);

		for (auto lastPos = m_Text.find_first_not_of(delim, 0), pos = m_Text.find_first_of(delim, lastPos);
			m_Text.npos != pos || m_Text.npos != lastPos;
			lastPos = m_Text.find_first_not_of(delim, pos), pos = m_Text.find_first_of(delim, lastPos)
			)
		{
			m_SegmentsView.emplace_back(&m_Underscore[lastPos], std::min(pos - lastPos, m_Underscore.size() - lastPos));
		}
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
		m_SegmentsView[idx].front() = '^';

		for (auto& c : m_SegmentsView[idx] | std::views::drop(1))
			c = '~';
	}

	constexpr void Underline(std::ptrdiff_t idx, char ch = '~') noexcept
	{
		std::ranges::fill(m_SegmentsView[idx], ch);
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
	constexpr auto GetSegment(std::ptrdiff_t idx) const noexcept -> string_view { return string_view{ m_SegmentsView[idx].data(), m_SegmentsView[idx].size() }; }

	string m_ErrorMsg{};

private:
	string_view m_Text{};
	string m_Underscore{};
	vector<span<char>> m_SegmentsView{};
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

	vector<instruction_t> m_Instructions{};
	shared_ptr<double> m_eax{ std::make_shared<double>() };
	shared_ptr<double> m_ebx{ std::make_shared<double>() };
	shared_ptr<double> m_ecx{ std::make_shared<double>() };
	shared_ptr<double> m_edx{ std::make_shared<double>() };

	// shared_ptr<uint16_t> m_ip{ std::make_shared<uint16_t>() };
	// shared_ptr<uint32_t> m_esp{ std::make_shared<uint32_t>() };
	// shared_ptr<uint32_t> m_ebp{ std::make_shared<uint32_t>() };

	// ESI
	// EDI

	shared_ptr<flag_register_t> m_eflags{ std::make_shared<flag_register_t>() };

	// Script parser

	auto Parser_GetDest(string_view argument) const noexcept -> expected<shared_ptr<double>, string_view>
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

	auto Parser_GetSrc(string_view argument) const noexcept -> expected<variant<shared_ptr<double>, double>, string_view>
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
				return (double)ret;

			return std::unexpected("Invalid immediate integer number");
		}

		double ret{};
		if (std::from_chars(argument.data(), argument.data() + argument.size(), ret).ec == std::errc{})
			return ret;

		return std::unexpected("Invalid immediate floating-point number");
	}

	auto Parser_ProcSig(span<string_view const> parameters, span<string_view const> arguments, string_view szLineText) const noexcept -> expected<vector<variant<shared_ptr<double>, double>>, vector<error_t>>
	{
		vector<variant<shared_ptr<double>, double>> processed{};
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
		assert((std::holds_alternative<decltype(m_eax)>(parsed_dest)));

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
		assert((std::holds_alternative<decltype(m_eax)>(parsed_lhs)));
		assert((std::holds_alternative<decltype(m_eax)>(parsed_rhs)));

		return
			[lhs{ std::get<0>(parsed_lhs) }, rhs{ std::get<0>(parsed_rhs) }]() noexcept
			{
				std::swap(*lhs, *rhs);
			};
	}

	using parser_t = decltype(&script_t::Parser_MOV);
	static inline constexpr std::pair<span<string_view const>, parser_t> PARSERS[] =
	{
		{ SIG_MOV, &script_t::Parser_MOV },
		{ SIG_XCHG, &script_t::Parser_XCHG },
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
		auto const arguments = UTIL_Split(szLine, " \t\f\v");
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
