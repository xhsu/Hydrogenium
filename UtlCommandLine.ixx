module;

#define HYDROGENIUM_COMMAND_LINE_VER 20241105L

#ifdef __INTELLISENSE__
#include <algorithm>
#include <expected>
#include <filesystem>
#include <format>
#include <functional>
#include <ranges>
#include <span>
#include <string_view>
#include <string>
#include <tuple>
#include <vector>
#endif

export module UtlCommandLine;

#ifndef __INTELLISENSE__
import std;
#endif

using std::expected;
using std::span;
using std::string;
using std::string_view;
using std::tuple;
using std::vector;

using std::uint8_t;

namespace fs = ::std::filesystem;


export enum struct ECmdRes : uint8_t
{
	OK = 0,
	BadCommand,
	BadArgCount,
	BadArgContent,
	Exception,
};

export vector<string_view> CommandLineGetArgs(int argc, char* argv[]) noexcept
{
	return span(argv, static_cast<size_t>(argc))
		| std::views::transform([](auto a) { return string_view{ a }; })
		| std::ranges::to<vector>();
}

export inline auto const CommandLineSliceArgs =
	std::views::chunk_by(+[](string_view const& lhs, string_view const& rhs) noexcept { return rhs[0] != '-'; })
	| std::views::filter([](auto&& r) noexcept { return !r.empty() && !r.front().empty(); });

export constexpr bool IsVariadicParameter(string_view param) noexcept
{
	return param.ends_with("...") || param.ends_with("...]");
}

export constexpr bool IsOptionalParameter(string_view param) noexcept
{
	return !param.empty()
		&& ((param.front() == '[' && param.back() == ']')
			|| IsVariadicParameter(param));
}

// #CONTINUE_FROM_HERE
export template <size_t N> constexpr auto StrVICmp(string_view sz, char const (&lit)[N]) noexcept -> decltype(uint8_t{} - uint8_t{})
{
	auto const case_ignored_eql =
		+[](char lhs, char rhs) noexcept
		{
			if (lhs >= 'A' && lhs <= 'Z')
				lhs = static_cast<char>(lhs - 'A' + 'a');
			if (rhs >= 'A' && rhs <= 'Z')
				rhs = static_cast<char>(rhs - 'A' + 'a');

			return lhs == rhs;
		};

	auto it1 = std::ranges::begin(sz), end1 = std::ranges::end(sz);
	auto it2 = std::ranges::begin(lit), end2 = std::ranges::end(lit);

	while (it1 != end1 && it2 != end2 && case_ignored_eql(*it1, *it2))
	{
		++it1;
		++it2;
	}

	auto const c1 = it1 == end1 ? '\0' : *it1;
	auto const c2 = it2 == end2 ? '\0' : *it2;

	return std::bit_cast<uint8_t>(c1) - std::bit_cast<uint8_t>(c2);
}

export constexpr bool TextToBoolean(string_view sz) noexcept
{
	return
		StrVICmp(sz.data(), "true") == 0
		|| StrVICmp(sz.data(), "yes") == 0;
}

export constexpr bool CommandLineGoodParameter(string_view parameter) noexcept
{
	// 0 - Not an parameter? Good!
	if (auto const ColonCount = std::ranges::count(parameter, ':'); ColonCount == 0)
		return true;

	// a. Must be exactly 1 colon present.
	else if (ColonCount >= 2)
		return false;

	if (IsOptionalParameter(parameter) && !IsVariadicParameter(parameter))
		parameter = parameter.substr(1, parameter.length() - 1);

	auto const pos = parameter.find_first_of(':');

	// b. label and restrain must be present.
	if (pos == 0 || pos >= (parameter.size() - 1))
		return false;

	auto const restrain = parameter.substr(0, pos);
	auto const identifier = parameter.substr(pos + 1);

	bool const bLegitRestrain =
		StrVICmp(restrain, "path") == 0 || StrVICmp(restrain, "dir") == 0 || StrVICmp(restrain, "file") == 0
		|| (restrain.contains('|') && restrain.front() != '|' && restrain.back() != '|');	// Enumerators

	auto const fnGoodIdentifier =
		[](char c) noexcept
		{
			return
				(c >= 'A' && c <= 'Z')
				|| (c >= 'a' && c <= 'z')
				|| (c >= '0' && c <= '9')
				|| (c == '_');
		};
	bool const bLegitIdentifier = std::ranges::fold_left(identifier | std::views::transform(fnGoodIdentifier), true, std::logical_and<>{});

	return bLegitIdentifier && bLegitRestrain;
}

export expected<void, string_view> ParameterArgumentMatching(string_view parameter, string_view argument) noexcept
{
	auto colon_pos = parameter.find_first_of(':');

	if (colon_pos == parameter.npos)	// No restrain.
		return {};

	// a. Not going to fully check the sanity again - skipping the colon counter.
	// b. Restrain must be in good place.
	if (colon_pos == 0 || colon_pos == (parameter.size() - 1))
		return std::unexpected("Bad parameter");

	if (parameter.front() == '[' && parameter.back() == ']')
		parameter = parameter.substr(1, parameter.length() - 2), --colon_pos;
	if (parameter.ends_with("..."))
		parameter = parameter.substr(0, parameter.length() - 3);

	auto const restrain = parameter.substr(0, colon_pos);
	auto const identifier = parameter.substr(colon_pos + 1);

	if (restrain.empty())	// No restrain.
		return {};

	// Path Types

	bool const bDir = StrVICmp(restrain, "dir") == 0;
	bool const bFile = StrVICmp(restrain, "file") == 0;
	bool const bPath = StrVICmp(restrain, "path") == 0 || bDir || bFile;

	if (bPath)
	{
		fs::path const ArgPath{ argument };

		if (!fs::exists(ArgPath))
			return std::unexpected("Path does not exist");

		if (bDir && !fs::is_directory(ArgPath))
			return std::unexpected("Argument is not a directory");

		return {};
	}

	// Enum Types

	if (restrain.contains('|'))
	{
		if (restrain.front() == '|' || restrain.back() == '|')
			return std::unexpected("Bad enum separator");

		bool bMatch{ false };
		for (auto lastPos = restrain.find_first_not_of('|', 0), pos = restrain.find_first_of('|', lastPos);
			restrain.npos != pos || restrain.npos != lastPos;
			lastPos = restrain.find_first_not_of('|', pos), pos = restrain.find_first_of('|', lastPos)
			)
		{
			if (restrain.substr(lastPos, pos - lastPos) == argument)
			{
				bMatch = true;
				break;
			}
		}

		if (!bMatch)
			return std::unexpected("Value not listed in enum");

		return {};
	}

	return {};
}

export string CommandLineGetParamTagText(string_view parameter) noexcept
{
	auto colon_pos = parameter.find_first_of(':');

	if (colon_pos == parameter.npos)	// No restrain.
		return string{ parameter };

	// a. Not going to fully check the sanity again - skipping the colon counter.
	// b. Restrain must be in good place.
	if (colon_pos == 0 || colon_pos == (parameter.size() - 1))
		return string{ parameter };

	bool FOptional = false;
	if (parameter.front() == '[' && parameter.back() == ']')
		parameter = parameter.substr(1, parameter.length() - 2), --colon_pos, FOptional = true;
	if (parameter.ends_with("..."))
		parameter = parameter.substr(0, parameter.length() - 3), FOptional = true;

	auto const restrain = parameter.substr(0, colon_pos);
	auto const identifier = parameter.substr(colon_pos + 1);

	if (restrain.empty())	// No restrain.
		return std::format("{1}{0}{2}", identifier, FOptional ? "[" : "", FOptional ? "]" : "");

	return std::format("{1}{0}{2}", restrain, FOptional ? "[" : "", FOptional ? "]" : "");
}

export constexpr bool CommandLineParamSanity(span<string_view const> expected) noexcept
{
	// 1. must be a valid array, and the command must starts with '-'
	if (expected.empty() || expected.front().empty() || expected.front()[0] != '-')
		return false;

	// 2. there shall be no required argument after optional arguments.
	auto optional_removed =
		expected
		| std::views::reverse
		| std::views::drop_while(&IsOptionalParameter)
		| std::views::reverse;

	// 3. only one variadic argument can be present, and it shall be the last argument
	auto const iVariadicArgCount = std::ranges::count_if(expected, &IsVariadicParameter);

	// 4. check for parameter sanity
	bool const bParamGood = std::ranges::fold_left(expected | std::views::transform(&CommandLineGoodParameter), true, std::logical_and<>{});

	return
		(iVariadicArgCount == 0 || iVariadicArgCount == 1 && IsVariadicParameter(expected.back()))
		&& std::ranges::count_if(optional_removed, &IsOptionalParameter) == 0;
}

export extern "C++" ECmdRes CommandLineWrapper(string_view desc, span<string_view const> expected, span<string_view const> received, void(*pfn)(span<string_view const>)) noexcept;

export extern "C++" void CommandLineUnknownInput(span<string_view const> arg_list) noexcept;

export extern "C++" void CommandLineRun(int argc, char* argv[], span<tuple<span<string_view const>, void(*)(span<string_view const>), string_view> const> descriptor) noexcept;

export extern "C++" void CommandLineUnitTest() noexcept;
