#include "Precompiled.hpp"

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
	fmt::print(
		Style::Skipping,
		"Compiled with: MSVC {}\nC++ {}L\nApplication Version: {}\n\n",
		_MSC_FULL_VER, _MSVC_LANG, __DATE__	// Or from application.ixx
	);

	constexpr auto max_len = std::ranges::max(
		CMD_HANDLER
		| std::views::elements<0>
		| std::views::transform(&span<string_view const>::front)
		| std::views::transform(&string_view::length)
	);

	for (auto&& [params, pfn, desc] : CMD_HANDLER)
	{
		fmt::print(Style::Action, "{}\n", desc);
		fmt::print(Style::Name, "\t{}", params[0]);
		fmt::print(Style::Info, "{}", string(max_len - params[0].length(), ' '));

		for (auto&& param : params | std::views::drop(1))
			fmt::print(IsOptionalParameter(param) ? Style::Skipping : Style::Info, " {}", param);

		fmt::print(Style::Info, "\n\n");
	}
}

static_assert(
	// Compile-time sanity check - make our life easier.
	[]() consteval -> bool
	{
		for (auto&& params : CMD_HANDLER | std::views::elements<0>)
		{
			if (!CommandLineParamSanity(params))
				return false;
		}

		return true;
	}()
);
#pragma endregion Command line stuff

int main(int argc, char* argv[]) noexcept
{
	CommandLineRun(argc, argv, CMD_HANDLER);
	CommandLineUnitTest();
}
