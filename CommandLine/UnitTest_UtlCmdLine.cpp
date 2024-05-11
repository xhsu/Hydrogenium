#include "Precompiled.hpp"

#include "UtlString.hpp"

import UtlCommandLine;
import Style;

using std::span;
using std::string;
using std::string_view;
using std::tuple;
using std::vector;

#pragma region Command line stuff
inline constexpr string_view ARG_DESC_HELP[] = { "-help" };

extern void ShowHelp(span<string_view const>) noexcept;

// #UPDATE_AT_CPP26 span over initializer list
inline constexpr tuple<span<string_view const>, void(*)(span<string_view const>), string_view> CMD_HANDLER[] =
{
	{ ARG_DESC_HELP, &ShowHelp, "Show all commands of this application." },
};

void ShowHelp(span<string_view const>) noexcept
{
	fmt::print(Style::Info, "\n");

	constexpr auto max_len = std::ranges::max(
		CMD_HANDLER
		| std::views::elements<0>
		| std::views::transform(&span<string_view const>::front)
		| std::views::transform(&string_view::length)
	);

	for (auto&& [arg_desc, pfn, desc] : CMD_HANDLER)
	{
		fmt::print(Style::Action, "{}\n", desc);
		fmt::print(Style::Name, "\t{}", arg_desc[0]);
		fmt::print(Style::Info, "{}", string(max_len - arg_desc[0].length(), ' '));

		for (auto&& arg : arg_desc | std::views::drop(1))
			fmt::print(IsOptionalArgument(arg) ? Style::Skipping : Style::Info, " {}", arg);

		fmt::print(Style::Info, "\n\n");
	}
}

static_assert(
	// Compile-time sanity check - make our life easier.
	[]() consteval -> bool
	{
		for (auto&& arg_desc : CMD_HANDLER | std::views::elements<0>)
		{
			if (!CommandLineArgSanity(arg_desc))
				return false;
		}

		return true;
	}()
);
#pragma endregion Command line stuff

int main(int argc, char* argv[]) noexcept
{
	//CommandLineRun(argc, argv, CMD_HANDLER);
	//CommandLineUnitTest();
	using namespace Hydrogenium::String::UnitTest;

	fmt::println("{}", StrI::Cmp(u8"你好", u8"你好"));
	fmt::println("{}", StrI::Cmp(u8"你好", u8"你好嗎"));
	fmt::println("{}", Mbs::Cnt(u8"هرقل"));

	Hydrogenium::StringPolicy::Advancing::UnitTest_as_multibytes_t_FWD();
}
