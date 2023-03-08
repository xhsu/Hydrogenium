import <math.h>;
import <stddef.h>;
import <stdint.h>;

import <algorithm>;
import <functional>;
import <iterator>;
import <limits>;
import <memory>;
import <optional>;
import <ranges>;
import <stdexcept>;
import <string_view>;
import <string>;

struct mysv
{
	using Traits_t = std::char_traits<char>;
	using Iter_t = const char*;
	using RevIter_t = std::reverse_iterator<Iter_t>;

	static inline constexpr auto npos = std::numeric_limits<ptrdiff_t>::max();

	template <size_t N>
	constexpr mysv(const char(&rgsz)[N]) noexcept : m_psz(&rgsz[0]), m_iSize(N), m_len(N - 1), m_count(recount()) { for (; m_psz[m_len] == '\0'; --m_len) {} ++m_len; }
	constexpr mysv(char const* psz, ptrdiff_t const iSize) noexcept : m_psz(psz), m_iSize(iSize), m_len(iSize), m_count(recount()) {}
	constexpr mysv(std::string const& sz) noexcept : mysv(sz.data(), sz.size()) {}
	constexpr mysv(std::string_view const& sz) noexcept : mysv(sz.data(), sz.size()) {}

	inline constexpr bool operator== (mysv const& rhs) const noexcept { return length() == rhs.length() && Traits_t::compare(m_psz, rhs.m_psz, length()) == 0; }
	inline constexpr auto operator<=> (mysv const& rhs) const noexcept { return Traits_t::compare(m_psz, rhs.m_psz, length()) <=> 0; }

	inline constexpr operator std::string_view() const noexcept { return std::string_view{ m_psz, m_iSize }; }
	inline constexpr operator std::string() const noexcept { return std::string(m_psz, m_iSize); }

	// Iterators

	inline constexpr Iter_t begin() const noexcept { return m_psz; }
	inline constexpr Iter_t end() const noexcept { return m_psz + length(); }
	inline constexpr RevIter_t rbegin() const noexcept { return std::make_reverse_iterator(end()); }
	inline constexpr RevIter_t rend() const noexcept { return std::make_reverse_iterator(begin()); }

	// Element access

	inline constexpr char operator[] (ptrdiff_t const pos) const noexcept { return m_psz[locate(pos)]; }
	inline constexpr char at(ptrdiff_t const pos) const { if (auto const idx = locate(pos); idx >= length()) throw std::out_of_range("StringView::at(): index out of range"); else return m_psz[idx]; }
	inline constexpr char front() const noexcept { return m_psz[0]; }
	inline constexpr char back() const noexcept { return m_psz[length() - 1]; }
	inline constexpr const char* data() const noexcept { return m_psz; }

	// Element access: EXT

	inline constexpr ptrdiff_t locate(ptrdiff_t const pos) const noexcept { return pos >= 0 ? pos : length() + pos; }
	inline constexpr ptrdiff_t grapheme_locate(ptrdiff_t const gf_pos) const noexcept { return gf_pos >= 0 ? gf_pos : count() + gf_pos; }
	inline constexpr bool grapheme_is_pos_encode(ptrdiff_t const pos) const noexcept { auto& c = m_psz[locate(pos)]; return (0 <= c && c <= 127) || (c & 0b11100000) == 0b11000000 || (c & 0b11110000) == 0b11100000 || (c & 0b11111000) == 0b11110000; }
	inline constexpr ptrdiff_t grapheme_index(ptrdiff_t gf_pos) const noexcept
	{
		gf_pos = grapheme_locate(gf_pos);

		auto iStrCount = 0;
		auto it = begin();
		auto const last = end();

		for (; it != last; ++iStrCount)
		{
			if (iStrCount == gf_pos)
				return std::distance(begin(), it);

			if (auto& c = *it; c < 0b10000000 && c >= 0)
			{
				++it;
			}
			else if ((c & 0b11100000) == 0b11000000)
			{
				it += 2;
			}
			else if ((c & 0b11110000) == 0b11100000)
			{
				it += 3;
			}
			else if ((c & 0b11111000) == 0b11110000)
			{
				it += 4;
			}
			else
			{
				// Broken UTF8
				std::terminate();
				std::unreachable();
			}
		}

		return npos;
	}
	inline constexpr mysv grapheme(ptrdiff_t const gf_pos) const noexcept
	{
		auto const idx = grapheme_index(gf_pos);
		auto const first = begin() + idx;

		if (auto& c = *first; c < 0b10000000 && c >= 0)
		{
			return mysv{ first, 1 };
		}
		else if ((c & 0b11100000) == 0b11000000)
		{
			return mysv{ first, 2 };
		}
		else if ((c & 0b11110000) == 0b11100000)
		{
			return mysv{ first, 3 };
		}
		else if ((c & 0b11111000) == 0b11110000)
		{
			return mysv{ first, 4 };
		}
		else
		{
			// Broken UTF8
			std::terminate();
			std::unreachable();

			return "";
		}
	}
	inline constexpr mysv grapheme_at(ptrdiff_t pos) const noexcept
	{
		pos = locate(pos);

		if (auto& c = m_psz[pos]; c < 0b10000000 && c >= 0)
		{
			return mysv{ &m_psz[pos], 1 };
		}
		else if ((c & 0b11100000) == 0b11000000)
		{
			return mysv{ &m_psz[pos], 2 };
		}
		else if ((c & 0b11110000) == 0b11100000)
		{
			return mysv{ &m_psz[pos], 3 };
		}
		else if ((c & 0b11111000) == 0b11110000)
		{
			return mysv{ &m_psz[pos], 4 };
		}
		else
		{
			// Broken UTF8
			std::terminate();
			std::unreachable();

			return mysv{ "" };
		}
	}
	inline constexpr mysv grapheme_front() const noexcept { return grapheme_at(0); }
	inline constexpr mysv grapheme_back() const noexcept { auto i = length() - 1; for (; !grapheme_is_pos_encode(i); --i) {} return grapheme_at(i); }

	// Capacity

	inline constexpr size_t size() const noexcept { return m_iSize; }
	inline constexpr ptrdiff_t ssize() const noexcept { return m_iSize > (size_t)std::numeric_limits<ptrdiff_t>::max() ? std::numeric_limits<ptrdiff_t>::max() : (ptrdiff_t)m_iSize; }
	inline constexpr ptrdiff_t length() const noexcept { return m_len; }
	static constexpr auto max_size() noexcept { return npos - 1; }	// #INVESTIGATE what does this mean??
	inline constexpr bool empty() const noexcept { return length() == 0; }

	// Capacity: EXT

	inline constexpr ptrdiff_t recount() const noexcept
	{
		ptrdiff_t iStrCount = 0;

		for (auto it = begin(); it != end(); ++iStrCount)
		{
			if (auto& c = *it; c < 0b10000000 && c >= 0)
			{
				++it;
			}
			else if ((c & 0b11100000) == 0b11000000)
			{
				it += 2;
			}
			else if ((c & 0b11110000) == 0b11100000)
			{
				it += 3;
			}
			else if ((c & 0b11111000) == 0b11110000)
			{
				it += 4;
			}
			else
			{
				// Broken UTF8
				std::terminate();
				std::unreachable();
			}
		}

		return iStrCount;
	}
	inline constexpr ptrdiff_t count() const noexcept { return m_count; }

	// Modifiers

	inline constexpr void remove_prefix(size_t const cnt) noexcept { m_psz += cnt; m_iSize -= cnt; m_len -= cnt; }
	inline constexpr void remove_suffix(size_t const cnt) noexcept { m_iSize -= cnt; m_len -= cnt; }
	inline constexpr void swap(mysv& rhs) noexcept { std::swap(*this, rhs); }

	// Operations

	inline constexpr auto copy(char* pDest, ptrdiff_t const cnt, ptrdiff_t const pos = 0) const { if (auto const idx = locate(pos); idx >= length()) throw std::out_of_range("StringView::copy(): index out of range"); else return Traits_t::copy(pDest, data() + idx, std::min(cnt, length() - idx)); }
	inline constexpr mysv substr(ptrdiff_t const pos, ptrdiff_t const cnt = npos) const { if (auto const idx = locate(pos); idx >= length()) throw std::out_of_range("StringView::substr(): index out of range"); else return mysv{ data() + idx, std::min(cnt, length() - idx) }; }
	inline constexpr bool starts_with(mysv const& sv) const noexcept { return Slice({}, sv.length()) == sv; }
	inline constexpr bool ends_with(mysv const& sv) const noexcept { return Slice(-sv.length(), {}) == sv; }
	inline constexpr bool contains(mysv const& sv) const noexcept { return std::ranges::contains_subrange(*this, sv); }
	inline constexpr bool contains(char const c) const noexcept { return std::ranges::contains(*this, c); }
	inline constexpr ptrdiff_t find(mysv const& sv, ptrdiff_t const pos = 0) const noexcept
	{
		auto first1 = begin() + locate(pos);
		auto const first2 = sv.begin(), last1 = end(), last2 = sv.end();

		for (;; ++first1)
		{
			for (auto it1 = first1, it2 = first2;; ++it1, ++it2)
			{
				if (it2 == last2)
					return std::distance(begin(), first1);
				if (it1 == last1)
					return npos;
				if (*it1 != *it2)
					break;
			}
		}

		return npos;
	}
	inline constexpr ptrdiff_t rfind(mysv const& sv, ptrdiff_t const pos = npos) const noexcept
	{
		auto first1 = std::make_reverse_iterator(begin() + std::min<ptrdiff_t>(locate(pos), length() - 1));
		auto const last1 = rend();
		auto const first2 = sv.begin(), last2 = sv.end();

		for (;; ++first1)
		{
			for (auto it1 = first1.base(), it2 = first2;; ++it1, ++it2)
			{
				if (it2 == last2)
					return std::distance(begin(), first1.base());
				if (it1 == last1.base())
					return npos;
				if (*it1 != *it2)
					break;
			}
		}

		return npos;
	}
	inline constexpr ptrdiff_t find_first_of(mysv const& sv, ptrdiff_t const pos = 0) const noexcept
	{
		for (auto it = begin() + locate(std::min(pos, length() - 1)); it != end(); ++it)
		{
			if (sv.contains(*it))
				return std::distance(begin(), it);
		}

		return npos;
	}
	inline constexpr ptrdiff_t find_last_of(mysv const& sv, ptrdiff_t const pos = npos) const noexcept
	{
		auto it = std::make_reverse_iterator(begin() + locate(std::min(pos, length() - 1) + 1));	// end() == rbegin(). Unlike '_first_of' series, '_last_of' is inclusive, i.e. [0, pos] will be the search range.
		auto const last = rend();

		for (; it != last; ++it)
		{
			if (sv.contains(*it))
				return std::distance(begin(), it.base()) - 1;	// make up for the offset between reverse_iterator and its corresponding iterator.
		}

		return npos;
	}
	inline constexpr ptrdiff_t find_first_not_of(mysv const& sv, ptrdiff_t const pos = 0) const noexcept
	{
		for (auto it = begin() + locate(std::min(pos, length() - 1)); it != end(); ++it)
		{
			if (!sv.contains(*it))
				return std::distance(begin(), it);
		}

		return npos;
	}
	inline constexpr ptrdiff_t find_last_not_of(mysv const& sv, ptrdiff_t const pos = npos) const noexcept
	{
		auto it = std::make_reverse_iterator(begin() + locate(std::min(pos, length() - 1) + 1));	// end() == rbegin(). Unlike '_first_of' series, '_last_of' is inclusive, i.e. [0, pos] will be the search range.
		auto const last = rend();

		for (; it != last; ++it)
		{
			if (!sv.contains(*it))
				return std::distance(begin(), it.base()) - 1;	// make up for the offset between reverse_iterator and its correspounding iterator.
		}

		return npos;
	}

	// Operations: EXT
	// #UPDATE_AT_CPP23 operator[] with multiple parameters.

	inline constexpr mysv Slice(std::optional<ptrdiff_t> const& pos1, std::optional<ptrdiff_t> const& pos2) const noexcept
	{
		auto const iSrc = pos1.transform(std::bind_front(&mysv::locate, this)).value_or(0);
		auto const iEnd = pos2.transform(std::bind_front(&mysv::locate, this)).value_or(length());

		[[unlikely]]
		if (iSrc > length() || iSrc < 0 || iEnd > length() || iEnd < 0 || iSrc > iEnd)
			std::terminate();

		return mysv{ m_psz + iSrc, iEnd - iSrc };
	}
	inline constexpr std::string Slice(std::optional<ptrdiff_t> const& pos1, std::optional<ptrdiff_t> const& pos2, std::optional<ptrdiff_t> const& step) const noexcept
	{
		auto const iStep = step.value_or(1);

		switch (iStep)
		{
			[[unlikely]]
		case 0:
			std::terminate();
			return {};

		case 1:
			return this->Slice(pos1, pos2);

			[[likely]]
		default:
			if (iStep > 0)
			{
				return this->Slice(pos1, pos2) | std::views::stride(iStep) | std::ranges::to<std::string>();
			}
			else
			{
				auto const iRevSrc = pos2.transform(std::bind_front(&mysv::locate, this)).value_or(-1) + 1;	// cap at 0
				auto const iRevEnd = pos1.transform(std::bind_front(&mysv::locate, this)).value_or(length() - 1) + 1;	// cap at length()

				return this->Slice(iRevSrc, iRevEnd) | std::views::reverse | std::views::stride(-iStep) | std::ranges::to<std::string>();
			}
		}

		std::unreachable();
	}
	inline constexpr std::string as_upper() const noexcept
	{
		return *this
			| std::views::transform([](char const c) constexpr /*static*/ noexcept { if (c >= 0x61 && c <= 0x7A) return static_cast<char>(c - 0x20); else return c; })
			| std::ranges::to<std::string>();
	}
	inline constexpr std::string as_lower() const noexcept
	{
		return *this
			| std::views::transform([](char const c) constexpr /*static*/ noexcept { if (c >= 0x41 && c <= 0x5A) return static_cast<char>(c + 0x20); else return c; })
			| std::ranges::to<std::string>();
	}

	// Sugar functions

	__forceinline constexpr bool starts_with(char const c) const noexcept { return front() == c; }
	__forceinline constexpr bool ends_with(char const c) const noexcept { return back() == c; }
	__forceinline constexpr auto find_first_of(char const c, ptrdiff_t const pos = 0) const noexcept { return find_first_of(mysv{ std::addressof(c), 1 }, pos); }
	__forceinline constexpr auto find_last_of(char const c, ptrdiff_t const pos = npos) const noexcept { return find_last_of(mysv{ std::addressof(c), 1 }, pos); }
	__forceinline constexpr auto find_first_not_of(char const c, ptrdiff_t const pos = 0) const noexcept { return find_first_not_of(mysv{ std::addressof(c), 1 }, pos); }
	__forceinline constexpr auto find_last_not_of(char const c, ptrdiff_t const pos = npos) const noexcept { return find_last_not_of(mysv{ std::addressof(c), 1 }, pos); }

	// Members: not private for using as template parameter.

	char const* m_psz{};
	size_t m_iSize{};
	ptrdiff_t m_len{};
	ptrdiff_t m_count{};
};

struct u8iter_t final
{
	using u8cell_t = std::string_view;

	using difference_type = ptrdiff_t;
	using value_type = u8cell_t;
	using pointer = const char*;
	using reference = const u8cell_t&;
	using iterator_category = std::bidirectional_iterator_tag;

	constexpr u8iter_t() noexcept = default;
	explicit constexpr u8iter_t(const char* psz) noexcept : m_psz(psz) {};
	constexpr u8iter_t(u8iter_t const& rhs) noexcept = default;
	constexpr u8iter_t& operator= (u8iter_t const& rhs) noexcept = default;

	static constexpr size_t width(char const c) noexcept
	{
		if (c < 0b10000000 && c >= 0)
		{
			return 1;
		}
		else if ((c & 0b11100000) == 0b11000000)
		{
			return 2;
		}
		else if ((c & 0b11110000) == 0b11100000)
		{
			return 3;
		}
		else if ((c & 0b11111000) == 0b11110000)
		{
			return 4;
		}
		else
		{
			// Not an UTF8 start pos.
			return 0;
		}
	}
	inline constexpr size_t width() const noexcept { return width(*m_psz); }
	static constexpr bool is_utf8_start(char const c) noexcept { return (0 <= c && c <= 127) || (c & 0b11100000) == 0b11000000 || (c & 0b11110000) == 0b11100000 || (c & 0b11111000) == 0b11110000; }
	inline constexpr bool is_valid() const noexcept { return is_utf8_start(*m_psz); }

	inline constexpr u8cell_t operator* () const noexcept { return u8cell_t{ m_psz, width() }; }
	inline constexpr std::unique_ptr<u8cell_t> operator-> () const noexcept { return std::make_unique<u8cell_t>(m_psz, width()); }

	// O(n) operation - not included. (Expected: O(1))
	inline constexpr u8cell_t operator[] (ptrdiff_t ofs) const noexcept
	{
		const char* p = m_psz;

		if (ofs > 0)
		{
			for (; ofs > 0 && is_utf8_start(*p); --ofs, p += width()) {}
		}
		else if (ofs < 0)
		{
			for (; ofs < 0 && is_utf8_start(*p); ++ofs)
			{
				for (--p; !is_utf8_start(*p); --p) {}
			}
		}

		return u8cell_t{ p, width(*p) };
	}

	inline constexpr u8iter_t& operator++ () noexcept { for (++m_psz; *m_psz && !width(); ++m_psz) {} return *this; }
	inline constexpr u8iter_t& operator-- () noexcept { for (--m_psz; *m_psz && !width(); --m_psz) {} return *this; }
	inline constexpr u8iter_t operator++ (int) noexcept { auto const cpy = *this; for (++m_psz; *m_psz && !width(); ++m_psz) {} return cpy; }
	inline constexpr u8iter_t operator-- (int) noexcept { auto const cpy = *this; for (--m_psz; *m_psz && !width(); --m_psz) {} return cpy; }

	// O(n) operation - not included. (Expected: O(1))
	inline constexpr u8iter_t& operator+= (ptrdiff_t ofs) noexcept
	{
		if (ofs > 0)
		{
			for (; ofs > 0 && is_valid(); --ofs, m_psz += width()) {}
		}
		else if (ofs < 0)
		{
			for (; ofs < 0 && is_valid(); ++ofs)
			{
				for (--m_psz; !is_valid(); --m_psz) {}
			}
		}

		return *this;
	}
	inline constexpr u8iter_t& operator-= (ptrdiff_t ofs) noexcept { return this->operator+=(-ofs); }
	inline constexpr u8iter_t operator+ (ptrdiff_t const ofs) const noexcept { auto cpy = *this; cpy += ofs; return cpy; }
	inline constexpr u8iter_t operator- (ptrdiff_t const ofs) const noexcept { auto cpy = *this; cpy -= ofs; return cpy; }

	// O(n) operation - but useful. (Expected: O(1))
	inline constexpr ptrdiff_t operator- (u8iter_t const& rhs) const noexcept
	{
		auto it = this->m_psz < rhs.m_psz ? this->m_psz : rhs.m_psz;	// starting from smaller one and goes to the greater one.
		auto const last = this->m_psz < rhs.m_psz ? rhs.m_psz : this->m_psz;
		ptrdiff_t cnt{};

		for (; it != last; it += width(*it))
		{
			++cnt;
		}

		return this->m_psz < rhs.m_psz ? -cnt : cnt;
	}

	inline constexpr bool operator== (u8iter_t const& rhs) const noexcept { return m_psz == rhs.m_psz; }
	inline constexpr auto operator<=> (u8iter_t const& rhs) const noexcept { return m_psz <=> rhs.m_psz; }

	const char* m_psz{};
};

struct utf8_string_view_t final
{

};

void UnitTest_UTF8Iterator() noexcept
{
	static constexpr u8iter_t it{u8"こんにちは"};

	static_assert(std::bidirectional_iterator<u8iter_t>);

	static_assert(u8iter_t::width(u8"A"[0]) == 1);	// 0x41
	static_assert(u8iter_t::width(u8"Á"[0]) == 2);	// 0xC3 0x81
	static_assert(u8iter_t::width(u8"Ḁ"[0]) == 3);	// 0xE1 0xB8 0x80
	static_assert(u8iter_t::width(u8"𐒰"[0]) == 4);	// 0xF0 0x90 0x92 0xB0

	static_assert(*it == u8"こ");
	static_assert(it->substr() == u8"こ");

	static constexpr auto fnTestBidirection = []() constexpr /*static*/ noexcept
	{
		u8iter_t it{ u8"こんにちは" };
		auto const first = it, last = first + 5;

		if (first[0] != u8"こ" || last[-1] != u8"は")
			return false;
		if (first[1] != u8"ん" || last[-2] != u8"ち")
			return false;
		if (first[2] != u8"に" || last[-3] != u8"に")
			return false;
		if (first[3] != u8"ち" || last[-4] != u8"ん")
			return false;
		if (first[4] != u8"は" || last[-5] != u8"こ")
			return false;

		if (*it++ != u8"こ" || it - first != 1)
			return false;
		if (*it++ != u8"ん" || it - first != 2)
			return false;
		if (*it++ != u8"に" || it - first != 3)
			return false;
		if (*it++ != u8"ち" || it - first != 4)
			return false;
		if (*it++ != u8"は" || it - first != 5)
			return false;

		if (*--it != u8"は" || it - last != -1)
			return false;
		if (*--it != u8"ち" || it - last != -2)
			return false;
		if (*--it != u8"に" || it - last != -3)
			return false;
		if (*--it != u8"ん" || it - last != -4)
			return false;
		if (*--it != u8"こ" || it - last != -5)
			return false;

		return true;
	};
	static_assert(fnTestBidirection());
}

void UnitTest_ASCIIStringView(void) noexcept
{
	static constexpr mysv szWord{ "word\0" };
	static constexpr mysv szRevWord { "drow\0" };

	static_assert(szWord[0] == 'w');
	static_assert(szWord[1] == 'o');
	static_assert(szWord[2] == 'r');
	static_assert(szWord[3] == 'd');
	static_assert(szWord[-1] == 'd');
	static_assert(szWord[-2] == 'r');
	static_assert(szWord[-3] == 'o');
	static_assert(szWord[-4] == 'w');

	static_assert(szWord.size() == 6);
	static_assert(szWord.ssize() == 6);
	static_assert(szWord.length() == 4);

	static_assert(*szWord.begin() == 'w');
	static_assert(*szWord.end() == '\0');   // technically it shouldn't be able to deref.
	static_assert(std::ranges::equal(szWord.rbegin(), szWord.rend(), szRevWord.begin(), szRevWord.end()));

	static_assert(szWord.front() == 'w');
	static_assert(szWord.back() == 'd');

	static constexpr mysv sz{ "fuck my life" };

	static_assert(sz == "fuck my life\0\0");
	static_assert(sz != "abc");
	static_assert(sz != "fuck me life");

	static_assert(sz.Slice({}, 4) == "fuck");
	static_assert(sz.Slice(5, 7) == "my");
	static_assert(sz.Slice(5, {}) == "my life");
	static_assert(sz.Slice(8, -1) == "lif");
	static_assert(sz.Slice({}, {}) == "fuck my life\0\0\0");

	static_assert(sz.Slice({}, {}, {}) == "fuck my life\0\0\0");
	static_assert(sz.Slice({}, {}, 2) == "fc ylf");
	static_assert(sz.Slice({}, 4, 2) == "fc");
	static_assert(sz.Slice({}, 4, 3) == "fk");
	static_assert(sz.Slice({}, 4, 4) == "f");
	static_assert(sz.Slice(8, -1, 3) == "l");
	static_assert(sz.Slice(8, {}, 3) == "le");	// fmt::print("sz.Slice(8, {{}}, 3) == '{}' (excepting: 'le')\n", sz.Slice(8, {}, 3));

	static_assert(sz.Slice(5, 7).Slice({}, {}, -1) == "ym");
	static_assert(sz.Slice(3, {}, -1) == "kcuf");	// fmt::print("sz.Slice(3, {{}}, -1) == '{}' (excepting: 'kcuf')\n", sz.Slice(3, {}, -1));
	static_assert(sz.Slice({}, -5, -1) == "efil");	// fmt::print("sz.Slice({{}}, -5, -1) == '{}' (excepting: 'efil')\n", sz.Slice({}, -5, -1));
	static_assert(szWord.Slice({}, {}, -1) == "drow");	// fmt::print("szWord.Slice({{}}, {{}}, -1) == '{}' (excepting: 'drow')\n", szWord.Slice({}, {}, -1));

	static constexpr auto fnTestOperatorCmp = []() constexpr /*static*/ noexcept -> bool
	{
		std::vector<mysv> rgszAns{ "assert", "exception", "functional", "iterator", "limits", "optional", "ranges", "string", "string_view" };
		std::vector<mysv> rgsz{ "exception", "string", "string_view", "limits", "optional", "functional", "iterator", "ranges", "assert" };

		if (rgsz == rgszAns)
			std::terminate();

		std::ranges::sort(rgsz);
		return rgsz == rgszAns;
	};
	static_assert(fnTestOperatorCmp());

	static constexpr auto fnTestPfxSfx = []() constexpr /*static*/ noexcept -> bool
	{
		mysv sz{ "   trim me" };
		sz.remove_prefix(3);

		char arr[] = { 'a', 'b', 'c', 'd', '\0', '\0', '\0' };
		mysv arr_view{ arr, sizeof(arr) };
		arr_view.remove_suffix(3);

		return sz == "trim me" && arr_view == "abcd";
	};
	static_assert(fnTestPfxSfx());

	static constexpr auto fnTestSwap = []() constexpr /*static*/ noexcept -> bool
	{
		mysv sz1{ "text1" }, sz2{ "words #2" };
		sz1.swap(sz2);

		return sz1 == "words #2" && sz2 == "text1";
	};
	static_assert(fnTestSwap());

	static constexpr auto fnTestCopy = []() constexpr /*static*/ noexcept -> bool
	{
		mysv source{ "ABCDEF" };
		char dest[3][8]{};
		size_t cnt{}, pos{};

		source.copy(&dest[0][0], cnt = 4);
		source.copy(&dest[1][0], cnt = 4, pos = 1);
		source.copy(&dest[2][0], cnt = 42, pos = 2);

		return mysv{ dest[0] } == "ABCD" && mysv { dest[1] } == "BCDE" && mysv { dest[2] } == "CDEF";
	};
	static_assert(fnTestCopy());

	static_assert(sz.substr(0, 4) == "fuck");
	static_assert(sz.substr(5, 2) == "my");
	static_assert(sz.substr(8, 42) == "life");
	static_assert(szWord.substr(0) == "word");

	static_assert(sz.starts_with("fuck"));
	static_assert(sz.ends_with("life"));
	static_assert(sz.contains("fuck") && sz.contains("my") && sz.contains("life"));
	static_assert(szWord.contains("word"));

	static_assert(sz.find("fuck") == 0);
	static_assert(sz.find("my") == 5);
	static_assert(sz.find("life") == 8);
	static_assert(sz.find(" ") == 4);
	static_assert(sz.find(" ", 5) == 7);
	static_assert(sz.find(" ", 8) == mysv::npos);
	static_assert(sz.find(" ", -5) == 7);

	static_assert(mysv{ "AB AB AB" }.rfind("AB") == 6);
	static_assert(mysv{ "AB AB AB" }.rfind("AB", 5) == 3);
	static_assert(mysv{ "B AB AB" }.rfind("AB", 2) == 2);
	static_assert(mysv{ "B AB AB" }.rfind("AB", 1) == mysv::npos);
	static_assert(mysv{ "B AB AB" }.rfind("A") == 5);
	static_assert(mysv{ "AB AB AB" }.rfind("B", 4) == 4);
	static_assert(mysv{ "AB AB AB" }.rfind("C") == mysv::npos);

	static constexpr auto N = mysv::npos;
	static_assert(
		1 == mysv{"alignas"}.find_first_of("klmn") &&
		//          └────────────────────────┘
		N == mysv{"alignof"}.find_first_of("wxyz") &&
		//
		3 == mysv{"concept"}.find_first_of("bcde", /* pos= */ 1) &&
		//            └──────────────────────┘
		N == mysv{"consteval"}.find_first_of("oxyz", /* pos= */ 2) &&
		//
		6 == mysv{"constexpr"}.find_first_of('x') &&
		//               └────────────────────┘
		N == mysv{"constinit"}.find_first_of('x') &&
		//
		6 == mysv{"const_cast"}.find_first_of('c', /* pos= */ 4) &&
		//               └─────────────────────┘
		N == mysv{"continue"}.find_first_of('c', /* pos= */ 42) &&
		//
		5 == mysv{"co_await"}.find_first_of("cba", /* pos= */ 4) &&
		//              └──────────────────────┘
		7 == mysv{"decltype"}.find_first_of("def", /* pos= */ 2)
		//                └───────────────────┘
		);

	static_assert(
		5 == mysv{"delete"}.find_last_of("cdef") &&
		//              └───────────────────┘
		N == mysv{"double"}.find_last_of("fghi") &&
		//
		0 == mysv{"else"}.find_last_of("bcde", 2 /* pos [0..2]: "els" */) &&
		//         └───────────────────────┘
		N == mysv{"explicit"}.find_last_of("abcd", 4 /* pos [0..4]: "expli" */) &&
		//
		3 == mysv{"extern"}.find_last_of('e') &&
		//            └───────────────────┘
		N == mysv{"false"}.find_last_of('x') &&
		//
		0 == mysv{"inline"}.find_last_of('i', 2 /* pos [0..2]: "inl" */) &&
		//         └──────────────────────┘
		N == mysv{"mutable"}.find_last_of('a', 2 /* pos [0..2]: "mut" */) &&
		//
		3 == mysv{"namespace"}.find_last_of("cdef", 3 /* pos [0..3]: "name" */)
		//            └────────────────────────┘
		);

	static_assert(2 == mysv{"BCDEF"}.find_first_not_of("ABC"));
	//                         ^
	static_assert(4 == mysv{"BCDEF"}.find_first_not_of("ABC", 4));
	//                           ^
	static_assert(1 == mysv{"BCDEF"}.find_first_not_of('B'));
	//                        ^
	static_assert(3 == mysv{"BCDEF"}.find_first_not_of('D', 2));
	//                          ^
	static_assert(1 == mysv{"BCDEF"}.find_last_not_of("DEF"));
	//                        ^
	static_assert(2 == mysv{"BCDEFG"}.find_last_not_of("EFG", 3));
	//                         ^
	static_assert(2 == mysv{"ABBA"}.find_last_not_of('A'));
	//                         ^
	static_assert(1 == mysv{"ABBA"}.find_last_not_of('A', 1));
	//                        ^

	static_assert(sz.as_upper() == "FUCK MY LIFE");
	static_assert(szWord.as_upper() == "WORD");
	static_assert(mysv{ "hELlO WoRLd!!" }.as_lower() == "hello world!!");

	static constexpr mysv szUTF{ u8"你好，UTF8作業系統！" };

	static_assert(szUTF.count() == 12);
	static_assert(szUTF.recount() == 12);

	static_assert(szUTF.grapheme(0) == u8"你");
	static_assert(szUTF.grapheme(1) == u8"好");
	static_assert(szUTF.grapheme(3) == u8"U");
	static_assert(szUTF.grapheme(7) == u8"作");
	static_assert(szUTF.grapheme(-1) == u8"！");
	static_assert(szUTF.grapheme(-2) == u8"統");
	static_assert(szUTF.grapheme(-7) == u8"F");

	static_assert(szUTF.grapheme_at(0) == u8"你");
	static_assert(szUTF.grapheme_at(3) == u8"好");
	static_assert(szUTF.grapheme_at(9) == u8"U");
	static_assert(szUTF.grapheme_at(13) == u8"作");
	static_assert(szUTF.grapheme_at(-3) == u8"！");
	static_assert(szUTF.grapheme_at(-6) == u8"統");
	static_assert(szUTF.grapheme_at(-17) == u8"F");

	static_assert(szUTF.grapheme_front() == u8"你");
	static_assert(szUTF.grapheme_back() == u8"！");

	static_assert(szUTF.grapheme_is_pos_encode(0));	// 你
	static_assert(!szUTF.grapheme_is_pos_encode(1));
	static_assert(!szUTF.grapheme_is_pos_encode(2));
	static_assert(szUTF.grapheme_is_pos_encode(3));	// 好
	static_assert(!szUTF.grapheme_is_pos_encode(4));
	static_assert(!szUTF.grapheme_is_pos_encode(5));
	static_assert(szUTF.grapheme_is_pos_encode(6));	// ，
	static_assert(!szUTF.grapheme_is_pos_encode(7));
	static_assert(!szUTF.grapheme_is_pos_encode(8));
	static_assert(szUTF.grapheme_is_pos_encode(9));	// U
	static_assert(szUTF.grapheme_is_pos_encode(10));	// T
	static_assert(szUTF.grapheme_is_pos_encode(11));	// F
	static_assert(szUTF.grapheme_is_pos_encode(12));	// 8
	static_assert(szUTF.grapheme_is_pos_encode(13));	// 作
	static_assert(!szUTF.grapheme_is_pos_encode(14));
}
