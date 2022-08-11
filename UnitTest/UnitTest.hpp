#ifndef HYDROGENIUM_UNIT_TEST_HEADER
#define HYDROGENIUM_UNIT_TEST_HEADER
#pragma once

#include <source_location>

#include <fmt/color.h>

inline void Log(const auto& sz, std::source_location hSourceLocation = std::source_location::current()) noexcept
{
	fmt::print(
		fmt::emphasis::bold | fmt::emphasis::italic | fg(fmt::color::yellow),
		"[{}] {}:({}, {}): {}\n",
		hSourceLocation.file_name(), hSourceLocation.function_name(), hSourceLocation.line(), hSourceLocation.column(),
		sz
	);
}

#endif	// ifdef HYDROGENIUM_UNIT_TEST_HEADER
