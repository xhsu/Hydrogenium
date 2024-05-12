module;

#define HYDROGENIUM_COMMAND_LINE_VER 20240428L


#include <algorithm>
#include <functional>
#include <ranges>
#include <span>
#include <string_view>
#include <tuple>
#include <vector>


export module UtlCommandLine;


using std::span;
using std::string;
using std::string_view;
using std::tuple;
using std::vector;


export enum struct ECmdRes : uint8_t
{
	OK = 0,
	Mismatch,
	BadArgs,
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

export constexpr bool IsVariadicArgument(string_view arg) noexcept
{
	return arg.ends_with("...") || arg.ends_with("...]");
}

export constexpr bool IsOptionalArgument(string_view arg) noexcept
{
	return !arg.empty()
		&& ((arg.front() == '[' && arg.back() == ']')
			|| IsVariadicArgument(arg));
}

// #CONTINUE_FROM_HERE
export template <size_t N> constexpr auto StrVNICmp(string_view sz, char const (&lit)[N]) noexcept -> decltype(uint8_t{} - uint8_t{})
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

export inline bool TextToBoolean(string_view sz) noexcept
{
	return
		_strnicmp(sz.data(), "true", std::min<size_t>(4, sz.length())) == 0
		|| _strnicmp(sz.data(), "yes", std::min<size_t>(3, sz.length())) == 0;
}

export constexpr bool CommandLineGoodParameter(string_view parameter) noexcept
{
	// 0 - Not an parameter? Good!
	if (auto const ColonCount = std::ranges::count(parameter, ':'); ColonCount == 0)
		return true;

	// a. Must be exactly 1 colon present.
	else if (ColonCount >= 2)
		return false;

	if (IsOptionalArgument(parameter) && !IsVariadicArgument(parameter))
		parameter = parameter.substr(1, parameter.length() - 1);

	auto const pos = parameter.find_first_of(':');

	// b. label and restrain must be present.
	if (pos == 0 || pos == (parameter.size() - 1))
		return false;

	auto const restrain = parameter.substr(0, pos);
	auto const identifier = parameter.substr(pos + 1);

	bool const bLegitRestrain =
		StrVNICmp(restrain, "path") == 0 || StrVNICmp(restrain, "dir") == 0 || StrVNICmp(restrain, "file")
		|| (restrain.contains('|') && restrain.front() != '|' && restrain.back() != '|');

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

export bool ParameterArgumentMatching(string_view parameter, string_view argument) noexcept
{
//#CONTINUE_FROM_HERE
	return true;
}

export constexpr bool CommandLineArgSanity(span<string_view const> expected) noexcept
{
	// 1. must be a valid array, and the command must starts with '-'
	if (expected.empty() || expected.front().empty() || expected.front()[0] != '-')
		return false;

	// 2. there shall be no required argument after optional arguments.
	auto optional_removed = expected
		| std::views::reverse
		| std::views::drop_while(&IsOptionalArgument) | std::views::reverse;

	// 3. only one variadic argument can be present, and it shall be the last argument
	auto const iVariadicArgCount = std::ranges::count_if(expected, &IsVariadicArgument);

	// 4. check for parameter sanity
	bool const bParamGood = std::ranges::fold_left(expected | std::views::transform(&CommandLineGoodParameter), true, std::logical_and<>{});

	return
		(iVariadicArgCount == 0 || iVariadicArgCount == 1 && IsVariadicArgument(expected.back()))
		&& std::ranges::count_if(optional_removed, &IsOptionalArgument) == 0;
}

export extern "C++" ECmdRes CommandLineWrapper(string_view desc, span<string_view const> expected, span<string_view const> received, void(*pfn)(span<string_view const>)) noexcept;

export extern "C++" void CommandLineUnknownInput(span<string_view const> arg_list) noexcept;

export extern "C++" void CommandLineRun(int argc, char* argv[], span<tuple<span<string_view const>, void(*)(span<string_view const>), string_view> const> descriptor) noexcept;

export extern "C++" void CommandLineUnitTest() noexcept;
