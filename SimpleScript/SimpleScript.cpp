// SimpleScript.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#ifdef __INTELLISENSE__
#include <__msvc_all_public_headers.hpp>
#else
import std.compat;
#endif

using std::expected;
using std::move_only_function;
using std::shared_ptr;
using std::span;
using std::string_view;
using std::variant;
using std::vector;

inline constexpr std::string_view SCRIPT = u8R"(
MOV EAX 100
MOV EBX EAX
MOV X Y
)";

inline constexpr std::string_view SIG_MOV[] = { "MOV", "%out", "%in" };

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

		return std::unexpected("Not valid storage");
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
			int32_t ret{};
			if (std::from_chars(argument.data(), argument.data() + argument.size(), ret).ec == std::errc{})
				return (double)ret;

			return std::unexpected("Invalid immediate integer number");
		}

		double ret{};
		if (std::from_chars(argument.data(), argument.data() + argument.size(), ret).ec == std::errc{})
			return ret;

		return std::unexpected("Invalid immediate floating-point number");
	}

	auto Parser_ProcSig(span<string_view const> parameters, span<string_view const> arguments) const noexcept -> expected<vector<variant<shared_ptr<double>, double, string_view>>, string_view>
	{
		if (arguments.size() != parameters.size())
			return std::unexpected("Wrong argument count");

		vector<variant<shared_ptr<double>, double, string_view>> processed{ parameters[0] };
		for (int i = 1; i < std::ssize(arguments); ++i)
		{
			if (parameters[i] == "%in" || parameters[i] == "%inout")
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
					processed.emplace_back(std::move(res).error());
			}
			else if (parameters[i] == "%out")
			{
				if (auto res = Parser_GetDest(arguments[i]); res.has_value())
					processed.emplace_back(std::move(res).value());
				else
					processed.emplace_back(std::move(res).error());
			}
			else
				return std::unexpected("Unrecognized param in signature");
		}

		return std::move(processed);
	}

	auto Parser_MOV(span<string_view const> arguments) const noexcept -> expected<instruction_t, string_view>
	{
		auto res = Parser_ProcSig(SIG_MOV, arguments);
		if (!res)
			return std::unexpected(std::move(res).error());

		auto processed{ std::move(res).value() };
		auto& parsed_dest = processed[1];
		auto& parsed_src = processed[2];

		if (parsed_dest.index() == 2)
			return std::unexpected(std::get<2>(std::move(parsed_dest)));
		else if (parsed_dest.index() == 1)
			return std::unexpected("Argument #1 of 'MOV' must be writable");

		if (parsed_src.index() == 2)
			return std::unexpected(std::get<2>(std::move(parsed_src)));

		switch (parsed_src.index())
		{
		case 0:
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<0>(std::move(parsed_src)) }]() noexcept
				{
					*dest = *src;
				};
		case 1:
			return
				[dest{ std::get<0>(std::move(parsed_dest)) }, src{ std::get<1>(std::move(parsed_src)) }]() noexcept
				{
					*dest = src;
				};

		default:
			std::unreachable();
			break;
		}

		std::unreachable();
		return std::unexpected("assume(false) reached");
	}

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
	variant<int, float> v0{ 1.f };

	script_t script{};
	auto const rgszLines = UTIL_Split(SCRIPT, "\r\n");

	for (int i = 0; i < std::ssize(rgszLines); ++i)
	{
		auto& szLine = rgszLines[i];

		auto const arguments = UTIL_Split(szLine, " \t\f\v");
		std::println("{}", szLine);

		if (arguments.front() == SIG_MOV[0])
		{
			auto res = script.Parser_MOV(arguments);
			if (!res)
				std::println("Compiler error:\n    Line {}: {}", i, std::move(res).error());
			else
				script.m_Instructions.emplace_back(std::move(res).value());
		}
	}

	script.Execute();
	script.Print();
}
