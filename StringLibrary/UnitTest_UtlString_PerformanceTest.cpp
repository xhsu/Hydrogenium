﻿#include "Precompiled.hpp"
#include "UtlString.hpp"

struct Timer final
{
	Timer() noexcept = default;
	Timer(uint32_t count) noexcept : m_count{ count } {}
	~Timer() noexcept
	{
		auto const last =
			std::chrono::high_resolution_clock::now();

		auto const diff = last - m_start;
		auto const sec = diff.count() / 1e9 / m_count;

		if (sec < 1e-8)
			std::println(u8"Average time: {:.3f}ns", diff.count() / (double)m_count);
		else if (sec < 1e-5)
			std::println(u8"Average time: {:.3f}μs", diff.count() / 1e3 / m_count);
		else if (sec < 1e-2)
			std::println(u8"Average time: {:.3f}ms", diff.count() / 1e6 / m_count);
		else
			std::println(u8"Average time: {:.3f}s", diff.count() / 1e9 / m_count);
	}

	std::chrono::high_resolution_clock::time_point m_start{ std::chrono::high_resolution_clock::now() };
	uint32_t m_count{ 1 };
};

void UnitTest_UtlString_PerformanceTest() noexcept
{
	using namespace Hydrogenium;
	using namespace Hydrogenium::String;

	Timer t{};

	//wchar_t wcs[] = L"🠀🠁🠂🠃🠄🠅🠆🠇🠈🠉🠊🠋";
	//fmt::println("{}", u8"🠀🠁🠂🠃🠄🠅🠆🠇🠈🠉🠊🠋");

	std::vector<bool> buf{};
	constexpr auto TEST_COUNT = 0x100'000u;
	buf.reserve(TEST_COUNT * 2);


	{
		Timer const t2{ TEST_COUNT };

		for (auto i = 0; i < TEST_COUNT; ++i)
		{
			buf.push_back(WcsI::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡") == L"葡萄不吐葡萄皮");
			buf.push_back(WcsIR::PBrk(L"吃葡萄不吐葡萄皮", L"吐葡") == L"葡萄皮");
		}

		fmt::print("Test on Wcs - ");
	}

	buf.clear();

	{
		Timer const t2{ TEST_COUNT };

		for (auto i = 0; i < TEST_COUNT; ++i)
		{
			buf.push_back(MbsI::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡") == u8"葡萄不吐葡萄皮");
			buf.push_back(MbsIR::PBrk(u8"吃葡萄不吐葡萄皮", u8"吐葡") == u8"葡萄皮");
		}

		fmt::print("Test on Mbs - ");
	}

	buf.clear();

	{
		Timer const t2{ TEST_COUNT };

		for (auto i = 0; i < TEST_COUNT; ++i)
		{
			buf.push_back(MbsCSpn(u8"吃葡萄不吐葡萄皮", u8"吐葡") == 1);
			buf.push_back(MbsRCSpn(u8"吃葡萄不吐葡萄皮", u8"吐葡") == -3);
		}

		fmt::print("Test on Component style - ");
	}

	buf.clear();

	fmt::println("{}", buf._Myvec);
}