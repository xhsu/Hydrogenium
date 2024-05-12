#include "Precompiled.hpp"
#include "UtlString.hpp"


int main(int, char* []) noexcept
{
	using namespace Hydrogenium;
	using namespace Hydrogenium::String::UnitTest;

	static_assert(CType<char>::IsAlNum('a') && !CType<char>::IsAlNum('!'));
	static_assert(CType<unsigned char>::IsAlNum('a') && !CType<unsigned char>::IsAlNum('!'));
	static_assert(CType<signed char>::IsAlNum('a') && !CType<signed char>::IsAlNum('!'));
	static_assert(CType<char16_t>::IsAlNum('a') && !CType<char16_t>::IsAlNum('!'));
	static_assert(CType<wchar_t>::IsAlNum('a') && !CType<wchar_t>::IsAlNum('!'));
	static_assert(CType<char32_t>::IsAlNum('a') && !CType<char32_t>::IsAlNum('!'));

	fmt::println("{}", StrI::Cmp(u8"你好", u8"你好"));
	fmt::println("{}", StrI::Cmp(u8"你好", u8"你好嗎"));
	fmt::println("{}", Mbs::Cnt(u8"هرقل"));
	fmt::println(u8"<empty>: {}, 你好: {}; 你好嗎: {}", Mbs::Cnt(""), Mbs::Cnt(u8"你好"), Mbs::Cnt(u8"你好嗎"));
}
