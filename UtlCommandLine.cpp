#include <assert.h>

#ifdef __INTELLISENSE__
#include <algorithm>
#include <filesystem>
#include <ranges>
#include <source_location>
#include <span>
#include <stacktrace>
#include <string_view>
#include <string>
#include <tuple>
#endif

#include <fmt/color.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

// Spliting this file out from CommandLine.ixx for the sake of __INTELLISENSE__
#ifndef __INTELLISENSE__
import std;
#endif

#define HYDROGENIUM_COMMAND_LINE_VER 20241105L

import UtlCommandLine;
import Style;

using std::span;
using std::string;
using std::string_view;
using std::tuple;
using std::vector;

namespace fs = ::std::filesystem;



ECmdRes CommandLineWrapper(string_view desc, span<string_view const> expected, span<string_view const> received, void(*pfn)(span<string_view const>)) noexcept
{
	auto const iReceivedArgCount = std::ssize(received);
	auto const iOptionalArgCount = std::ranges::count_if(expected, &IsOptionalParameter);
	auto const iExpectedArgCount = std::ssize(expected);
	auto const bIsVariadic = std::ranges::count_if(expected, &IsVariadicParameter) > 0;
	auto const diff = iExpectedArgCount - iReceivedArgCount;	// < 0: more arg than needed; > 0: less arg than needed

	assert(iReceivedArgCount > 0 && iExpectedArgCount > 0);
	assert(CommandLineParamSanity(expected));

	if (iReceivedArgCount <= 0 || expected.front() != received.front())	// not calling function if the command won't match
		return ECmdRes::BadCommand;

	if ((diff < 0 && !bIsVariadic) || diff > iOptionalArgCount)
	{
		fmt::print(Style::Error, R"(Expected argument count for command "{}" is {}, but {} received.)" "\n", expected[0], iExpectedArgCount - 1, iReceivedArgCount - 1);

		string underscore{};

		fmt::print(Style::Info, "        [Desc] {}\n", desc);
		fmt::print(Style::Info, "        [Args] {} ", fmt::styled(expected.front(), Style::Action));
		underscore += string(fmt::formatted_size("        [Args] {} ", expected.front()), ' ');

		for (auto&& [Param, Arg] :
			std::views::zip(expected | std::views::drop(1), received | std::views::drop(1)))
		{
			auto const szTag = CommandLineGetParamTagText(Param);
			fmt::print(Style::Positive, "{}", szTag);
			fmt::print(Style::Info, ": ");
			fmt::print(Style::Action, "{}", Arg);
			fmt::print(Style::Info, ", ");

			underscore += string(fmt::formatted_size("{}: {}, ", szTag, Arg), ' ');
		}

		if (diff < 0)	// more arg than needed
		{
			for (auto&& Arg : received | std::views::drop(iExpectedArgCount))
			{
				fmt::print(Style::Skipping, "<unexpected>");
				fmt::print(Style::Info, ": ");
				fmt::print(Style::Warning, "{}", Arg);
				fmt::print(Style::Info, ", ");

				underscore += string(strlen("<unexpected>: "), '~');
				underscore += '^';
				underscore += string(Arg.length() - 1, '~');
				underscore += "  ";	// for ", "
			}
		}
		else if (diff > 0)	// less arg than needed
		{
			for (auto&& Param : expected | std::views::drop(iReceivedArgCount))
			{
				bool const bOptionalArg = IsOptionalParameter(Param);
				string_view const szRejected{ bOptionalArg ? "<optional>" : "<required>" };

				fmt::print(bOptionalArg ? Style::Skipping : Style::Warning, "{}", Param);
				fmt::print(bOptionalArg ? Style::Skipping : Style::Info, ": ");
				fmt::print(Style::Skipping, "{}", szRejected);
				fmt::print(Style::Info, ", ");

				underscore += '^';
				underscore += string(fmt::formatted_size("{}: {}", Param, szRejected) - 1, '~');
				underscore += "  ";	// for ", "
			}
		}

		underscore += '\n';
		fmt::print(Style::Info, "\n{}", underscore);
		return ECmdRes::BadArgCount;
	}

	// Now check into the content of the arguments
	else
	{
		auto parameters = expected | std::ranges::to<vector>();
		if (auto const iCount = std::ssize(received) - std::ssize(parameters); iCount > 0)
			parameters.append_range(std::views::repeat(expected.back()) | std::views::take(iCount));

		std::ptrdiff_t idx{ 1 };
		for (auto&& [Param, Arg] :
			std::views::zip(parameters | std::views::drop(1), received | std::views::drop(1)))
		{
			if (auto const res = ParameterArgumentMatching(Param, Arg); !res)
			{
				fmt::print(Style::Error, R"(Argument {} of "{}" fails its restrain: {}.)" "\nThe rest remains unchecked.\n", idx, parameters.front(), res.error());

				string underscore{};

				fmt::print(Style::Info, "        [Desc] {}\n", desc);
				fmt::print(Style::Info, "        [Args] {} ", fmt::styled(expected.front(), Style::Action));
				underscore += string(fmt::formatted_size("        [Args] {} ", expected.front()), ' ');

				for (std::ptrdiff_t j = 1; j < idx; ++j)
				{
					auto const szTag = CommandLineGetParamTagText(parameters[j]);
					fmt::print(Style::Positive, "{}", szTag);
					fmt::print(Style::Info, ": ");
					fmt::print(Style::Action, "{}", received[j]);
					fmt::print(Style::Info, ", ");

					underscore += string(fmt::formatted_size("{}: {}, ", szTag, received[j]), ' ');
				}

				fmt::print(Style::Skipping, "{}", Param);
				fmt::print(Style::Skipping, ": ");
				fmt::print(Style::Warning, "{}", Arg);
				fmt::print(Style::Info, ", ");

				underscore += string(fmt::formatted_size("{}: ", Param), '~');
				underscore += '^';
				underscore += string(Arg.length() - 1, '~');
				underscore += '\n';
				fmt::print(Style::Info, "\n{}", underscore);

				return ECmdRes::BadArgContent;
			}

			++idx;
		}
	}

	try
	{
		(*pfn)(received | std::views::drop(1));
	}
	catch (const std::exception& e)
	{
		auto const stack_trace = std::stacktrace::current();

		fmt::print(Style::Error, "[{}] Unhandled exception: {}\n", std::source_location::current().function_name(), e.what());
		fmt::print(Style::Info, "\tStack trace: {}\n", std::to_string(stack_trace));

		return ECmdRes::Exception;
	}
	catch (...)
	{
		auto const stack_trace = std::stacktrace::current();

		fmt::print(Style::Error, "[{}] Unhandled exception with unknown type.", std::source_location::current().function_name());
		fmt::print(Style::Info, "\tStack trace: {}\n", std::to_string(stack_trace));

		return ECmdRes::Exception;
	}

	return ECmdRes::OK;
}

void CommandLineUnknownInput(span<string_view const> arg_list) noexcept
{
	fmt::print(Style::Warning, u8R"(Unknown command ")");
	fmt::print(fmt::emphasis::italic | Style::Action, "{}", arg_list.front());
	fmt::print(Style::Warning, u8R"(" received.)");
	fmt::print(Style::Info, "\n");
	fmt::print(Style::Warning, "Input ignored.\n");

	string underscore{ fmt::format("        {0:~<{1}}", '^', arg_list.front().length())};
	fmt::print("        {}", fmt::styled(arg_list.front(), Style::Action));

	for (auto&& arg : arg_list | std::views::drop(1))
	{
		underscore += fmt::format(" {0:~<{1}}", '^', arg.length());
		fmt::print(Style::Skipping, " {}", arg);
	}

	fmt::print("\n");
	fmt::print("{}\n", underscore);
}

void CommandLineRun(int argc, char* argv[], span<tuple<span<string_view const>, void(*)(span<string_view const>), string_view>const> descriptor) noexcept
{
	auto const args = CommandLineGetArgs(argc, argv);

	// Drop the first one, it's the path to exe
	for (auto&& arguments : args | std::views::drop(1) | CommandLineSliceArgs)
	{
		bool bHandled = false;
		for (auto&& [parameters, pfn, desc] : descriptor)
		{
			// It's actually executing the callback function as long as it is not a mismatch.
			if (CommandLineWrapper(desc, parameters, arguments, pfn) != ECmdRes::BadCommand)
			{
				bHandled = true;
				break;
			}
		}

		if (!bHandled)
			CommandLineUnknownInput(arguments);
	}
}

void CommandLineUnitTest() noexcept
{
#ifdef _DEBUG
	static_assert(StrVICmp("ABC", "abc") == 0);
	static_assert(StrVICmp("ABCD", "abc") > 0);
	static_assert(StrVICmp("ABC", "abcd") < 0);

	static constexpr string_view testing_param_1{ "arg1" };
	static constexpr string_view testing_param_2{ "[arg2]" };
	static constexpr string_view testing_param_3{ "arg3..." };
	static constexpr string_view testing_param_4{ "[arg4...]" };
	static_assert(!IsVariadicParameter(testing_param_1) && !IsOptionalParameter(testing_param_1));
	static_assert(!IsVariadicParameter(testing_param_2) && IsOptionalParameter(testing_param_2));
	static_assert(IsVariadicParameter(testing_param_3) && IsOptionalParameter(testing_param_3));
	static_assert(IsVariadicParameter(testing_param_4) && IsOptionalParameter(testing_param_4));	// variadic implys optional, as the 0 arg possibility exists

	using std::vector;

	static constexpr string_view arg_sanity_check_0[] = { "sanity", };	// Command must start with '-'
	static constexpr string_view arg_sanity_check_1[] = { "-sanity", "arg1" };
	static constexpr string_view arg_sanity_check_2[] = { "-sanity", "arg1", "[opt1]" };
	static constexpr string_view arg_sanity_check_3[] = { "-sanity", "arg1", "[opt1]", "arg2" };	// Optional arg must be at tail
	static constexpr string_view arg_sanity_check_4[] = { "-sanity", "[var...]" };
	static constexpr string_view arg_sanity_check_5[] = { "-sanity", "arg1", "var1...", "[var2...]" };	// Only one variadic allowed
	static constexpr string_view arg_sanity_check_6[] = { "-sanity", "arg1", "[var]...", "[opt1]" };	// variadic must be at tail
	static constexpr string_view arg_sanity_check_7[] = { "-sanity", "arg1", "[opt1]", "[var]..." };
	static constexpr string_view arg_sanity_check_8[] = { "-sanity", "path:arg1", "dir:arg2", "file:arg3", "enum0|enum1|enum2:arg4", "[path:arg1]", "[dir:opt1]", "[file:opt3]", "[enum0|enum1:opt4]" };

	static_assert(!CommandLineParamSanity({}));
	static_assert(!CommandLineParamSanity(arg_sanity_check_0));
	static_assert(CommandLineParamSanity(arg_sanity_check_1));
	static_assert(CommandLineParamSanity(arg_sanity_check_2));
	static_assert(!CommandLineParamSanity(arg_sanity_check_3));
	static_assert(CommandLineParamSanity(arg_sanity_check_4));
	static_assert(!CommandLineParamSanity(arg_sanity_check_5));
	static_assert(!CommandLineParamSanity(arg_sanity_check_6));
	static_assert(CommandLineParamSanity(arg_sanity_check_7));
	static_assert(CommandLineParamSanity(arg_sanity_check_8));


	auto do_nothing = +[](span<string_view const> a) noexcept { fmt::println("Test case passed: {}", a); };
	vector<string_view> expected_1{ "-test1", "arg1", "[opt1]", };
	vector<string_view> args_1_a{ "-test1", };
	vector<string_view> args_1_b{ "-test1", "arg1", };
	vector<string_view> args_1_c{ "-test1", "arg1", "opt1", };
	vector<string_view> args_1_d{ "-test1", "arg1", "opt1", "ext1", };

	//auto test_case_1 =
	//	[&](vector<string_view> const& args, string_view desc) noexcept
	//	{
	//		return CommandLineWrapper(desc, expected_1, args, do_nothing);
	//	};

	assert(CommandLineWrapper("Lack of arg. Test expected to be failed.", expected_1, args_1_a, do_nothing) == ECmdRes::BadArgCount);
	assert(CommandLineWrapper("Ignoring optional arg. Test expected to be passed.", expected_1, args_1_b, do_nothing) == ECmdRes::OK);
	assert(CommandLineWrapper("Filling all args. Test expected to be passed.", expected_1, args_1_c, do_nothing) == ECmdRes::OK);
	assert(CommandLineWrapper("Overfeeding args. Test expected to be failed.", expected_1, args_1_d, do_nothing) == ECmdRes::BadArgCount);

	vector<string_view> expected_2{ "-test2", "arg1", "[opt1]", "[var]..." };
	vector<string_view> args_2_a{ "-test2", };
	vector<string_view> args_2_b{ "-test2", "arg1", };
	vector<string_view> args_2_c{ "-test2", "arg1", "opt1", };
	vector<string_view> args_2_d{ "-test2", "arg1", "opt1", "any1", };
	vector<string_view> args_2_e{ "-test2", "arg1", "opt1", "any1", "any2" };

	assert(CommandLineWrapper("Lack of arg. Test expected to be failed.", expected_2, args_2_a, do_nothing) == ECmdRes::BadArgCount);
	assert(CommandLineWrapper("Ignoring optional arg. Test expected to be passed.", expected_2, args_2_b, do_nothing) == ECmdRes::OK);
	assert(CommandLineWrapper("Filling optional arg. Test expected to be passed.", expected_2, args_2_c, do_nothing) == ECmdRes::OK);
	assert(CommandLineWrapper("Filling first variadic arg. Test expected to be passed.", expected_2, args_2_d, do_nothing) == ECmdRes::OK);
	assert(CommandLineWrapper("Filling second variadic arg. Test expected to be passed.", expected_2, args_2_e, do_nothing) == ECmdRes::OK);

	vector<string_view> expected_3{ "-test3", "path:arg1", "dir:arg2", "file:arg3", "[enum0|enum1|enum2:var...]" };
	vector<string_view> args_3_a{ "-test3", "Bin/", "bin/", "Hydrogenium.vcxproj", "enum0", "enum1", "enum2" };
	vector<string_view> args_3_b{ "-test3", "Bin/", "bin/", "Hydrogenium.vcxproj", "enum4", "asdf"};

	assert(CommandLineWrapper("Argument restraining test. Expected to be passed.", expected_3, args_3_a, do_nothing) == ECmdRes::OK);
	assert(CommandLineWrapper("Argument restraining test. Expected to be failed.", expected_3, args_3_b, do_nothing) == ECmdRes::BadArgContent);
#endif
}
