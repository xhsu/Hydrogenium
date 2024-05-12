#include "UtlString.hpp"

namespace Hydrogenium::UnitTest
{
	static_assert(CType<char>::CodePointOf(u8"A"[0]) == CodePoint::WHOLE);
	static_assert(CType<char>::CodePointOf(u8"Á"[0]) == CodePoint::BEGIN_OF_2);
	static_assert(CType<char>::CodePointOf(u8"あ"[0]) == CodePoint::BEGIN_OF_3);
	static_assert(CType<char>::CodePointOf(u8"あ"[1]) == CodePoint::MID);
	static_assert(CType<char>::CodePointOf(u8"𐒰"[0]) == CodePoint::BEGIN_OF_4);

	static_assert(CType<char>::ToFullWidth(u8"A") == U'A');
	static_assert(CType<char>::ToFullWidth(u8"Á") == U'Á');
	static_assert(CType<char>::ToFullWidth(u8"あ") == U'あ');
	static_assert(CType<char>::ToFullWidth(u8"𐒰") == U'𐒰');

	using U8MBARR = std::array<unsigned char, 4>;
	static_assert(CType<char>::ToMultiBytes(U'A') == U8MBARR{ 'A', 0, 0, 0 });
	static_assert(CType<char>::ToMultiBytes(U'Á') == U8MBARR{ 0xC3, 0x81, 0, 0 });
	static_assert(CType<char>::ToMultiBytes(U'あ') == U8MBARR{ 0xE3, 0x81, 0x82, 0 });
	static_assert(CType<char>::ToMultiBytes(U'𐒰') == U8MBARR{ 0xF0, 0x90, 0x92, 0xB0 });

	static_assert(CType<wchar_t>::CodePointOf(L"A"[0]) == CodePoint::WHOLE);
	static_assert(CType<wchar_t>::CodePointOf(L"Á"[0]) == CodePoint::WHOLE);
	static_assert(CType<wchar_t>::CodePointOf(L"あ"[0]) == CodePoint::WHOLE);
	static_assert(CType<wchar_t>::CodePointOf(L"𐒰"[0]) == CodePoint::BEGIN_OF_2);
	static_assert(CType<wchar_t>::CodePointOf(L"𐒰"[1]) == CodePoint::MID);

	static_assert(CType<wchar_t>::ToFullWidth(L"A") == U'A');
	static_assert(CType<wchar_t>::ToFullWidth(L"Á") == U'Á');
	static_assert(CType<wchar_t>::ToFullWidth(L"あ") == U'あ');
	static_assert(CType<wchar_t>::ToFullWidth(L"𐒰") == U'𐒰');

	using U16MBARR = std::array<wchar_t, 2>;
	static_assert(CType<wchar_t>::ToMultiBytes(U'A') == U16MBARR{ L'A', 0 });
	static_assert(CType<wchar_t>::ToMultiBytes(U'Á') == U16MBARR{ L'Á', 0 });
	static_assert(CType<wchar_t>::ToMultiBytes(U'あ') == U16MBARR{ L'あ', 0 });
	static_assert(CType<wchar_t>::ToMultiBytes(U'𐒰') == U16MBARR{ 0xD801, 0xDCB0 });
}