module;

//#define ENABLE_HSL_COLOR

// C
#include <cassert>

export module UtlColor;

// C
import <cstdint>;

// C++
import <algorithm>;	// std::clamp
import <array>;		// Using msvc array helper classes.
import <bit>;		// bit_cast
import <concepts>;	// std::integral...
import <tuple>;		// GetRGB returns a tuple

// Static math lib
import "gcem/include/gcem.hpp";

// Friend module
import UtlConcepts;


using std::array;
using std::bit_cast;


//-----------------------------------------------------------------------------
// Purpose: Basic handler for an rgb set of colors
//			This class is fully inline
//-----------------------------------------------------------------------------
export struct Color4b
{
	// constructors
	constexpr Color4b() noexcept : _color() { *((uint32_t*)this) = 0U; }
	constexpr Color4b(uint8_t r, uint8_t g, uint8_t b) noexcept : _color() { _color[0] = r; _color[1] = g; _color[2] = b; _color[3] = 0; }
	constexpr Color4b(uint8_t r, uint8_t g, uint8_t b, uint8_t a) noexcept : _color() { _color[0] = r; _color[1] = g; _color[2] = b; _color[3] = a; }
	constexpr Color4b(uint32_t ulRGB, uint8_t a) noexcept : _color() { _color[0] = (ulRGB & 0xFF0000) >> 16; _color[1] = (ulRGB & 0xFF00) >> 8; _color[2] = ulRGB & 0xFF; _color[3] = a; }
	constexpr Color4b(uint32_t color32) noexcept : _color() { SetRawColor(color32); }
	explicit constexpr Color4b(std::ranges::range auto&& RangeObj) noexcept : _color() { for (auto&& [Ref, Val] : ::ranges::views::zip(*this, RangeObj)) Ref = static_cast<uint8_t>(Val); }	// #UPDATE_AT_CPP26 ranges::enumerator

	// set the color
	// r - red component (0-255)
	// g - green component (0-255)
	// b - blue component (0-255)
	// a - alpha component, controls transparency (0 - transparent, 255 - opaque);
	constexpr void SetColor(std::integral auto r, std::integral auto g, std::integral auto b, std::integral auto a) noexcept
	{
		_color[0] = static_cast<uint8_t>(r);
		_color[1] = static_cast<uint8_t>(g);
		_color[2] = static_cast<uint8_t>(b);
		_color[3] = static_cast<uint8_t>(a);
	}

	constexpr void SetColor(std::floating_point auto r, std::floating_point auto g, std::floating_point auto b, std::floating_point auto a) noexcept
	{
		_color[0] = static_cast<uint8_t>(r * 255.0);
		_color[1] = static_cast<uint8_t>(g * 255.0);
		_color[2] = static_cast<uint8_t>(b * 255.0);
		_color[3] = static_cast<uint8_t>(a * 255.0);
	}

	constexpr void GetColor(std::integral auto& r, std::integral auto& g, std::integral auto& b, std::integral auto& a) const noexcept
	{
		r = static_cast<std::remove_reference_t<decltype(r)>>(_color[0]);
		g = static_cast<std::remove_reference_t<decltype(g)>>(_color[1]);
		b = static_cast<std::remove_reference_t<decltype(b)>>(_color[2]);
		a = static_cast<std::remove_reference_t<decltype(a)>>(_color[3]);
	}

	constexpr void GetColor(std::floating_point auto& r, std::floating_point auto& g, std::floating_point auto& b, std::floating_point auto& a) const noexcept
	{
		r = static_cast<std::remove_reference_t<decltype(r)>>(_color[0]) / 255.0;
		g = static_cast<std::remove_reference_t<decltype(g)>>(_color[1]) / 255.0;
		b = static_cast<std::remove_reference_t<decltype(b)>>(_color[2]) / 255.0;
		a = static_cast<std::remove_reference_t<decltype(a)>>(_color[3]) / 255.0;
	}

	constexpr void SetRawColor(uint32_t hexColorAGBR) noexcept
	{
		*((uint32_t*)this) = hexColorAGBR;
	}

	constexpr void SetRawColor(uint32_t ulRGB, uint8_t a) noexcept
	{
		_color[0] = (ulRGB & 0xFF0000) >> 16;	// r
		_color[1] = (ulRGB & 0xFF00) >> 8;		// g
		_color[2] = ulRGB & 0xFF;				// b
		_color[3] = a;
	}

	constexpr uint32_t GetRawColor() const noexcept	// Returns 0xAABBGGRR
	{
		return *((uint32_t*)this);
	}

	constexpr uint32_t GetRawRGB() const noexcept	// Returns 0xRRGGBB
	{
		return static_cast<uint32_t>(_color[0] << 16 | _color[1] << 8 | _color[2]);
	}

	constexpr uint8_t& r() noexcept	{ return _color[0]; }	// #UPDATE_AT_CPP23 deduce this
	constexpr uint8_t& g() noexcept	{ return _color[1]; }
	constexpr uint8_t& b() noexcept	{ return _color[2]; }
	constexpr uint8_t& a() noexcept	{ return _color[3]; }

	constexpr uint8_t& operator[](std::integral auto index) noexcept { assert(index < _countof(_color)); return _color[index]; }
	constexpr const uint8_t operator[](std::integral auto index) const noexcept { assert(index < _countof(_color)); return _color[index]; }

	constexpr bool operator== (const Color4b& rhs) const noexcept { return (*((int*)this) == *((int*)&rhs)); }	// operator!= will be automatically generated in C++20.
	constexpr Color4b& operator=(const Color4b& rhs) { *((int*)this) = *((int*)&rhs); return *this; }

	constexpr decltype(auto) operator~() const noexcept { return Color4b(*((uint32_t*)this) ^ 0xFFFFFF); }	// Reversed color. It is easier on HEX calculation.

	// Iterators
	using value_type = uint8_t;
	using iterator = _STD _Array_iterator<value_type, 4>;
	using const_iterator = _STD _Array_const_iterator<value_type, 4>;
	using reverse_iterator = _STD reverse_iterator<iterator>;
	using const_reverse_iterator = _STD reverse_iterator<const_iterator>;
	[[nodiscard]] constexpr iterator begin(void) noexcept { return iterator(_color, 0); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator begin(void) const noexcept { return const_iterator(_color, 0); }
	[[nodiscard]] constexpr iterator end(void) noexcept { return iterator(_color, 4); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator end(void) const noexcept { return const_iterator(_color, 4); }
	[[nodiscard]] constexpr reverse_iterator rbegin(void) noexcept { return reverse_iterator(end()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rbegin(void) const noexcept { return const_reverse_iterator(end()); }
	[[nodiscard]] constexpr reverse_iterator rend(void) noexcept { return reverse_iterator(begin()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rend(void) const noexcept { return const_reverse_iterator(begin()); }
	[[nodiscard]] constexpr const_iterator cbegin(void) const noexcept { return begin(); }
	[[nodiscard]] constexpr const_iterator cend(void) const noexcept { return end(); }
	[[nodiscard]] constexpr const_reverse_iterator crbegin(void) const noexcept { return rbegin(); }
	[[nodiscard]] constexpr const_reverse_iterator crend(void) const noexcept { return rend(); }

	// Capacity
	[[nodiscard]] static consteval bool empty(void) noexcept { return false; }
	[[nodiscard]] static consteval std::size_t size(void) noexcept { return static_cast<std::size_t>(4); }
	[[nodiscard]] static consteval std::size_t max_size(void) noexcept { return static_cast<std::size_t>(4); }

private:
	uint8_t _color[4];
};

export struct Color4f
{
	constexpr Color4f() noexcept : _color{ 0, 0, 0, 0 } {}
	constexpr Color4f(std::integral auto r, std::integral auto g, std::integral auto b) noexcept : _color{ std::clamp(static_cast<double>(r) / 255.0, 0.0, 1.0), std::clamp(static_cast<double>(g) / 255.0, 0.0, 1.0), std::clamp(static_cast<double>(b) / 255.0, 0.0, 1.0), 0 } {}
	constexpr Color4f(std::integral auto r, std::integral auto g, std::integral auto b, std::integral auto a) noexcept : _color{ std::clamp(static_cast<double>(r) / 255.0, 0.0, 1.0), std::clamp(static_cast<double>(g) / 255.0, 0.0, 1.0), std::clamp(static_cast<double>(b) / 255.0, 0.0, 1.0), std::clamp(static_cast<double>(a) / 255.0, 0.0, 1.0) } {}
	constexpr Color4f(std::floating_point auto r, std::floating_point auto g, std::floating_point auto b) noexcept : _color{ std::clamp<double>(r, 0, 1), std::clamp<double>(g, 0, 1), std::clamp<double>(b, 0, 1), 0 } {}
	constexpr Color4f(std::floating_point auto r, std::floating_point auto g, std::floating_point auto b, std::floating_point auto a) noexcept : _color{ std::clamp<double>(r, 0, 1), std::clamp<double>(g, 0, 1), std::clamp<double>(b, 0, 1), std::clamp<double>(a, 0, 1) } {}
	constexpr Color4f(uint32_t ulRGB, uint8_t a) noexcept : _color{} { SetRawColor(ulRGB, a); }
	constexpr Color4f(uint32_t hexColorAGBR) noexcept : _color{} { SetRawColor(hexColorAGBR); }
	constexpr Color4f(const Color4b& color4ub) noexcept : _color{} { SetRawColor(color4ub); }
	explicit constexpr Color4f(std::ranges::range auto&& RangeObj) noexcept : _color() { for (auto&& [Ref, Val] : ::ranges::views::zip(*this, RangeObj)) Ref = static_cast<double>(Val); }	// #UPDATE_AT_CPP26 ranges::enumerator

	constexpr void SetRGB(std::floating_point auto& r, std::floating_point auto& g, std::floating_point auto& b) noexcept
	{
		_color[0] = std::clamp(static_cast<double>(r), 0.0, 1.0);
		_color[1] = std::clamp(static_cast<double>(g), 0.0, 1.0);
		_color[2] = std::clamp(static_cast<double>(b), 0.0, 1.0);
	}

	// set the color
	// r - red component (0-255)
	// g - green component (0-255)
	// b - blue component (0-255)
	// a - alpha component, controls transparency (0 - transparent, 255 - opaque);
	constexpr void SetRGB(std::integral auto r, std::integral auto g, std::integral auto b) noexcept
	{
		_color[0] = std::clamp(static_cast<double>(r) / 255.0, 0.0, 1.0);
		_color[1] = std::clamp(static_cast<double>(g) / 255.0, 0.0, 1.0);
		_color[2] = std::clamp(static_cast<double>(b) / 255.0, 0.0, 1.0);
	}

	constexpr std::tuple<uint8_t, uint8_t, uint8_t> GetRGB(void) const noexcept
	{
		return std::make_tuple(
			static_cast<uint8_t>(gcem::round(_color[0] * 255.0)),
			static_cast<uint8_t>(gcem::round(_color[1] * 255.0)),
			static_cast<uint8_t>(gcem::round(_color[2] * 255.0))
		);
	}

	constexpr void SetRawColor(uint32_t hexColorAGBR) noexcept
	{
		//color32_helper_t color;
		//color.hexColor = hexColorAGBR;

		auto const arr = bit_cast<array<uint8_t, 4>>(hexColorAGBR);

		//_color[0] = static_cast<double>(color.ubColors[0]) / 255.0;
		//_color[1] = static_cast<double>(color.ubColors[1]) / 255.0;
		//_color[2] = static_cast<double>(color.ubColors[2]) / 255.0;
		//_color[3] = static_cast<double>(color.ubColors[3]) / 255.0;
		_color[0] = static_cast<double>(arr[0]) / 255.0;
		_color[1] = static_cast<double>(arr[1]) / 255.0;
		_color[2] = static_cast<double>(arr[2]) / 255.0;
		_color[3] = static_cast<double>(arr[3]) / 255.0;
	}

	constexpr void SetRawColor(uint32_t ulRGB, uint8_t a) noexcept
	{
		_color[0] = static_cast<double>((ulRGB & 0xFF0000) >> 16) / 255.0;
		_color[1] = static_cast<double>((ulRGB & 0xFF00) >> 8) / 255.0;
		_color[2] = static_cast<double>(ulRGB & 0xFF) / 255.0;
		_color[3] = static_cast<double>(a) / 255.0;
	}

	constexpr void SetRawColor(const Color4b& color4ub) noexcept
	{
		_color[0] = static_cast<double>(color4ub[0]) / 255.0;
		_color[1] = static_cast<double>(color4ub[1]) / 255.0;
		_color[2] = static_cast<double>(color4ub[2]) / 255.0;
		_color[3] = static_cast<double>(color4ub[3]) / 255.0;
	}

	constexpr uint32_t GetRawColor(void) const noexcept	// Returns 0xAABBGGRR
	{
		return bit_cast<uint32_t>(array{ r, g, b, a });
	}

	constexpr uint32_t GetRawRGB(void) const noexcept	// Returns 0xRRGGBB
	{
		return static_cast<uint32_t>(r << 16 | g << 8 | b);
	}

	constexpr Color4b GetColor4ubObj(void) const noexcept
	{
		return Color4b(r, g, b, a);
	}

	constexpr void SetHSV(const double& h, const double& s, const double& v) noexcept	// HSV to RGB. H[0-360], S[0-1], V[0-1]
	{
		if (s < DBL_EPSILON)	// < is bogus, just shuts up warnings
		{
			_color[0] = v;
			_color[1] = v;
			_color[2] = v;

			return;
		}

		double hh = h;
		if (hh >= 360.0)
			hh = 0.0;

		hh /= 60.0;

		long i = static_cast<long>(hh);
		double ff = hh - i;
		double p = v * (1.0 - s);
		double q = v * (1.0 - (s * ff));
		double t = v * (1.0 - (s * (1.0 - ff)));

		switch (i)
		{
		case 0:
			_color[0] = v;
			_color[1] = t;
			_color[2] = p;
			break;

		case 1:
			_color[0] = q;
			_color[1] = v;
			_color[2] = p;
			break;

		case 2:
			_color[0] = p;
			_color[1] = v;
			_color[2] = t;
			break;

		case 3:
			_color[0] = p;
			_color[1] = q;
			_color[2] = v;
			break;

		case 4:
			_color[0] = t;
			_color[1] = p;
			_color[2] = v;
			break;

		case 5:
		default:
			_color[0] = v;
			_color[1] = p;
			_color[2] = q;
			break;
		}
	}

	constexpr std::tuple<double, double, double> GetHSV(void) const noexcept
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		if (delta < DBL_EPSILON || max < DBL_EPSILON)
		{
			// Hue is undefined when saturation is 0, i.e. achromatic situation.

			return std::make_tuple(
				std::numeric_limits<double>::quiet_NaN(),	// Hue
				0.0,	// Saturation
				max	// Brightness
			);
		}

		double _h = 60.0;	// hue is in degrees
		if (_color[0] >= max)					// > is bogus, just keeps compilor happy
			_h *= (_color[1] - _color[2]) / delta;	// between yellow & magenta
		else if (_color[1] >= max)
			_h *= 2.0 + (_color[2] - _color[0]) / delta;	// between cyan & yellow
		else
			_h *= 4.0 + (_color[0] - _color[1]) / delta;	// between magenta & cyan

		if (_h < 0.0)
			_h += 360.0;
		else if (_h >= 360.0)
			_h -= 360.0;	// hue should be [0, 360)

		return std::make_tuple(
			_h,	// Hue
			delta / max,	// Saturation
			max	// Brightness
		);
	}

	static constexpr Color4f HSV(const double& h, const double& s, const double& v) noexcept
	{
		Color4f obj;
		obj.SetHSV(h, s, v);
		return obj;
	}

#ifdef ENABLE_HSL_COLOR
	constexpr void SetHSL(const double& h, const double& s, const double& l) noexcept	// HSV to RGB. H[0-360], S[0-1], V[0-1]
	{
		if (s < DBL_EPSILON)
		{
			_color[0] = _color[1] = _color[2] = l; // achromatic
		}
		else
		{
			auto hue2rgb = [](const double& p, const double& q, double t)
			{
				if (t < DBL_EPSILON) t += 1.0;
				if (t > 1.0) t -= 1.0;
				if (t < 1.0 / 6.0) return p + (q - p) * 6.0 * t;
				if (t < 1.0 / 2.0) return q;
				if (t < 2.0 / 3.0) return p + (q - p) * (2.0 / 3.0 - t) * 6.0;
				return p;
			};

			auto q = l < 0.5 ? l * (1.0 + s) : l + s - l * s;
			auto p = 2.0 * l - q;
			auto t = h / 360.0;

			_color[0] = hue2rgb(p, q, t + 1.0 / 3.0);
			_color[1] = hue2rgb(p, q, t);
			_color[2] = hue2rgb(p, q, t - 1.0 / 3.0);
		}
	}

	constexpr void GetHSL(Arithmetic auto& h, std::floating_point auto& s, std::floating_point auto& l) const noexcept
	{
		using hueTy = std::remove_reference_t<decltype(h)>;
		using saturationTy = std::remove_reference_t<decltype(s)>;
		using lightnessTy = std::remove_reference_t<decltype(l)>;

		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		l = (max + min) / 2.0;

		if (delta < DBL_EPSILON)
		{
			h = s = 0; // achromatic
		}
		else
		{
			s = l > 0.5 ? delta / (2.0 - max - min) : delta / (max + min);

			if (max == _color[0])
				h = (_color[1] - _color[2]) / delta + (_color[1] < _color[2] ? 6.0 : 0);
			else if (max == _color[1])
				h = (_color[2] - _color[0]) / delta + 2.0;
			else if (max == _color[2])
				h = (_color[0] - _color[1]) / delta + 4.0;

			h /= 6.0;
			h *= 360.0;
		}
	}

	static constexpr Color4f HSL(const double& h, const double& s, const double& l) noexcept
	{
		Color4f obj;
		obj.SetHSL(h, s, l);
		return obj;
	}
#endif

	// Check function.
	constexpr void Rationalize() noexcept
	{
		_color[0] = std::clamp(_color[0], 0.0, 1.0);
		_color[1] = std::clamp(_color[1], 0.0, 1.0);
		_color[2] = std::clamp(_color[2], 0.0, 1.0);
		_color[3] = std::clamp(_color[3], 0.0, 1.0);
	}

	// Color component assigning functions.
	// RGB type.
	inline constexpr void SetR(std::integral auto R) noexcept { _color[0] = std::clamp(static_cast<double>(R) / 255.0, 0.0, 1.0); }
	inline constexpr void SetG(std::integral auto G) noexcept { _color[1] = std::clamp(static_cast<double>(G) / 255.0, 0.0, 1.0); }
	inline constexpr void SetB(std::integral auto B) noexcept { _color[2] = std::clamp(static_cast<double>(B) / 255.0, 0.0, 1.0); }
	inline constexpr void SetA(std::integral auto A) noexcept { _color[3] = std::clamp(static_cast<double>(A) / 255.0, 0.0, 1.0); }

	// HSV/HSL type.
	constexpr void SetH(Arithmetic auto _h) noexcept
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		SetHSV(
			std::clamp<decltype(_h)>(_h, 0, 360),
			max < DBL_EPSILON ? 0 : delta / max,
			max
		);
	}
	constexpr void SetS(std::floating_point auto _s) noexcept
	{
		if (_s < std::numeric_limits<decltype(_s)>::epsilon())
		{
			_color[0] = _color[1] = _color[2] = v;	// achromatic
			return;
		}

		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		double _h = 60;	// degrees

		if (r >= max)					// > is bogus, just keeps compilor happy
			_h *= (_color[1] - _color[2]) / delta;	// between yellow & magenta
		else if (g >= max)
			_h *= 2.0 + (_color[2] - _color[0]) / delta;	// between cyan & yellow
		else
			_h *= 4.0 + (_color[0] - _color[1]) / delta;	// between magenta & cyan

		if (_h < 0.0)
			_h += 360.0;

		SetHSV(
			_h,
			std::min<decltype(_s)>(_s, 1.0),
			max
		);
	}
	constexpr void SetV(std::floating_point auto _v) noexcept
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		double _h = 60;	// degrees

		if (r >= max)					// > is bogus, just keeps compilor happy
			_h *= (_color[1] - _color[2]) / delta;	// between yellow & magenta
		else if (g >= max)
			_h *= 2.0 + (_color[2] - _color[0]) / delta;	// between cyan & yellow
		else
			_h *= 4.0 + (_color[0] - _color[1]) / delta;	// between magenta & cyan

		if (_h < 0.0)
			_h += 360.0;

		SetHSV(
			_h,
			(delta <= DBL_EPSILON || max <= DBL_EPSILON) ? 0.0 : delta / max,
			std::clamp<decltype(_v)>(_v, 0, 1)
		);
	}
#ifdef ENABLE_HSL_COLOR
	constexpr void SetS_hsl(std::floating_point auto _s) noexcept
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		double _h = 60;	// degrees

		if (r >= max)					// > is bogus, just keeps compilor happy
			_h *= (_color[1] - _color[2]) / delta;	// between yellow & magenta
		else if (g >= max)
			_h *= 2.0 + (_color[2] - _color[0]) / delta;	// between cyan & yellow
		else
			_h *= 4.0 + (_color[0] - _color[1]) / delta;	// between magenta & cyan

		if (_h < 0.0)
			_h += 360.0;

		SetHSL(
			_h,
			std::clamp<decltype(_s)>(_s, 0, 1),
			(max + min) / 2.0
		);
	}
	constexpr void SetL(std::floating_point auto _l) noexcept
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		double _h = 60;	// degrees

		if (r >= max)					// > is bogus, just keeps compilor happy
			_h *= (_color[1] - _color[2]) / delta;	// between yellow & magenta
		else if (g >= max)
			_h *= 2.0 + (_color[2] - _color[0]) / delta;	// between cyan & yellow
		else
			_h *= 4.0 + (_color[0] - _color[1]) / delta;	// between magenta & cyan

		if (_h < 0.0)
			_h += 360.0;

		SetHSL(
			_h,
			delta < DBL_EPSILON ? 0 : delta / (1.0 - gcem::abs(max + min - 1.0)),	// Theoratically this would be abs(2L-1), but should I use the new lightness value?
			std::clamp<decltype(_l)>(_l, 0, 1)
		);
	}
#endif

	// Color component retrieve functions.
	// RGB type.
	inline constexpr uint8_t GetR() const noexcept { return static_cast<uint8_t>(gcem::round(_color[0] * 255.0)); }
	inline constexpr uint8_t GetG() const noexcept { return static_cast<uint8_t>(gcem::round(_color[1] * 255.0)); }
	inline constexpr uint8_t GetB() const noexcept { return static_cast<uint8_t>(gcem::round(_color[2] * 255.0)); }
	inline constexpr uint8_t GetA() const noexcept { return static_cast<uint8_t>(gcem::round(_color[3] * 255.0)); }

	__declspec(property(get = GetR, put = SetR)) uint8_t r;
	__declspec(property(get = GetG, put = SetG)) uint8_t g;
	__declspec(property(get = GetB, put = SetB)) uint8_t b;
	__declspec(property(get = GetA, put = SetA)) uint8_t a;

	// HSV/HSL type.
	constexpr double GetH() const noexcept	// Degree: [0-360]
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		if (delta < DBL_EPSILON)
			return std::numeric_limits<double>::quiet_NaN();	// In this case, h value is undefined.

		if (max < DBL_EPSILON)
		{
			// if max is 0, then r = g = b = 0              
			// s = 0, h is undefined
			return std::numeric_limits<double>::quiet_NaN();	// its now undefined
		}

		double _h = 60;	// degrees

		if (r >= max)					// > is bogus, just keeps compilor happy
			_h *= (_color[1] - _color[2]) / delta;	// between yellow & magenta
		else if (g >= max)
			_h *= 2.0 + (_color[2] - _color[0]) / delta;	// between cyan & yellow
		else
			_h *= 4.0 + (_color[0] - _color[1]) / delta;	// between magenta & cyan

		if (_h < 0.0)
			_h += 360.0;

		return _h;
	}
	constexpr double GetS() const noexcept	// Percentage: [0.0-1.0]
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		if (delta < DBL_EPSILON || max < DBL_EPSILON)
		{
			// delta == 0 means achromatic
			return 0.0;
		}

		return delta / max;	// NOTE: if Max is == 0, this divide would cause a crash
	}
	constexpr double GetV() const noexcept	// Percentage: [0.0-1.0]
	{
		return std::max({ _color[0], _color[1], _color[2] });	// v
	}
#ifdef ENABLE_HSL_COLOR
	constexpr double GetS_hsl() const noexcept
	{
		auto min = std::min({ _color[0], _color[1], _color[2] });
		auto max = std::max({ _color[0], _color[1], _color[2] });
		auto delta = max - min;

		return l > 0.5 ? delta / (2.0 - max - min) : delta / (max + min);
	}
	constexpr double GetL() const noexcept	// Percentage: [0.0-1.0]
	{
		return 0.5 * (std::min({ _color[0], _color[1], _color[2] }) + std::max({ _color[0], _color[1], _color[2] }));	// Use [0-1] floating color here.
	}
#endif

	__declspec(property(get = GetH, put = SetH)) double h;
	__declspec(property(get = GetS, put = SetS)) double s;
	__declspec(property(get = GetV, put = SetV)) double v;
#ifdef ENABLE_HSL_COLOR
	__declspec(property(get = GetS_hsl, put = SetS_hsl)) double s_hsl;
	__declspec(property(get = GetL, put = SetL)) double l;
#endif

	// Operators.
	// Retrieve these actually returns you the original floating color value.
	// [0 - R, 1 - G, 2 - B, 3 - A]
	constexpr double& operator[](std::integral auto index) noexcept { assert(index < 4); return ((double*)(&_color[0]))[index]; }
	constexpr const double operator[](std::integral auto index) const noexcept { assert(index < 4); return ((const double*)(&_color[0]))[index]; }

	constexpr bool operator== (const Color4f& rhs) const noexcept { return _color[0] == rhs._color[0] && _color[1] == rhs._color[1] && _color[2] == rhs._color[2] && _color[3] == rhs._color[3]; }	// Shame on C++, 'memcmp' should be a constexpr function.
	constexpr bool operator== (const Color4b& rhs) const noexcept { return GetRawColor() == rhs.GetRawColor(); }	// Operator!= will be automatically generated by C++20.

	constexpr Color4f& operator=(const Color4f& rhs) noexcept { _color[0] = rhs._color[0]; _color[1] = rhs._color[1]; _color[2] = rhs._color[2]; _color[3] = rhs._color[3]; return *this; }	// Shame on C++, 'memcpy' should be a constexpr function.
	constexpr Color4f& operator=(const Color4b& rhs) noexcept { SetRawColor(rhs); return *this; }

	constexpr decltype(auto) operator+(const Color4f& v) const noexcept { return Color4f(_color[0] + v._color[0], _color[1] + v._color[1], _color[2] + v._color[2]); }
	constexpr decltype(auto) operator-(const Color4f& v) const noexcept { return Color4f(_color[0] - v._color[0], _color[1] - v._color[1], _color[2] - v._color[2]); }
	constexpr decltype(auto) operator+=(const Color4f& v) noexcept { return (*this = *this + v); }
	constexpr decltype(auto) operator-=(const Color4f& v) noexcept { return (*this = *this - v); }

	constexpr decltype(auto) operator+(const Color4b& v) const noexcept { return *this + Color4f(v); }
	constexpr decltype(auto) operator-(const Color4b& v) const noexcept { return *this - Color4f(v); }
	constexpr decltype(auto) operator+=(const Color4b& v) noexcept { return (*this = *this + v); }
	constexpr decltype(auto) operator-=(const Color4b& v) noexcept { return (*this = *this - v); }

	constexpr decltype(auto) operator*(Arithmetic auto fl) const noexcept { return Color4f(_color[0] * fl, _color[1] * fl, _color[2] * fl); }
	constexpr decltype(auto) operator/(Arithmetic auto fl) const noexcept { return Color4f(_color[0] / fl, _color[1] / fl, _color[2] / fl); }
	constexpr decltype(auto) operator*=(Arithmetic auto fl) noexcept { return (*this = *this * fl); }
	constexpr decltype(auto) operator/=(Arithmetic auto fl) noexcept { return (*this = *this / fl); }

	constexpr decltype(auto) operator~() const noexcept { return Color4f(GetRawRGB() ^ 0xFFFFFF, a); }	// Inverse color. By definition it is the hue that actually 'reversed'. i.e. (hue + 180) % 360.

	// Iterators
	using value_type = double;
	using iterator = _STD _Array_iterator<value_type, 4>;
	using const_iterator = _STD _Array_const_iterator<value_type, 4>;
	using reverse_iterator = _STD reverse_iterator<iterator>;
	using const_reverse_iterator = _STD reverse_iterator<const_iterator>;
	[[nodiscard]] constexpr iterator begin(void) noexcept { return iterator(_color, 0); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator begin(void) const noexcept { return const_iterator(_color, 0); }
	[[nodiscard]] constexpr iterator end(void) noexcept { return iterator(_color, 4); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator end(void) const noexcept { return const_iterator(_color, 4); }
	[[nodiscard]] constexpr reverse_iterator rbegin(void) noexcept { return reverse_iterator(end()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rbegin(void) const noexcept { return const_reverse_iterator(end()); }
	[[nodiscard]] constexpr reverse_iterator rend(void) noexcept { return reverse_iterator(begin()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rend(void) const noexcept { return const_reverse_iterator(begin()); }
	[[nodiscard]] constexpr const_iterator cbegin(void) const noexcept { return begin(); }
	[[nodiscard]] constexpr const_iterator cend(void) const noexcept { return end(); }
	[[nodiscard]] constexpr const_reverse_iterator crbegin(void) const noexcept { return rbegin(); }
	[[nodiscard]] constexpr const_reverse_iterator crend(void) const noexcept { return rend(); }

	// Capacity
	[[nodiscard]] static consteval bool empty(void) noexcept { return false; }
	[[nodiscard]] static consteval std::size_t size(void) noexcept { return static_cast<std::size_t>(4); }
	[[nodiscard]] static consteval std::size_t max_size(void) noexcept { return static_cast<std::size_t>(4); }

private:
	double _color[4];
};

export inline constexpr bool operator== (const Color4b& lhs, const Color4f& rhs) noexcept { return rhs == lhs; }
export inline constexpr bool operator!= (const Color4b& lhs, const Color4f& rhs) noexcept { return rhs != lhs; }
export inline constexpr decltype(auto) operator+ (const Color4b& lhs, const Color4f& rhs) noexcept { return rhs + lhs; }
export inline constexpr decltype(auto) operator- (const Color4b& lhs, const Color4f& rhs) noexcept { return rhs - lhs; }
export inline constexpr decltype(auto) operator+= (Color4b& lhs, const Color4f& rhs) noexcept { return (lhs = (lhs + rhs).GetColor4ubObj()); }
export inline constexpr decltype(auto) operator-= (Color4b& lhs, const Color4f& rhs) noexcept { return (lhs = (lhs - rhs).GetColor4ubObj()); }
export inline constexpr decltype(auto) operator* (float fl, const Color4f& c) noexcept { return c * fl; }
export inline constexpr decltype(auto) operator/ (float fl, const Color4f& c) noexcept { return c / fl; }

// Helper concepts
export
template<typename T>
concept IsColor = std::same_as<std::decay_t<T>, Color4b> || std::same_as<std::decay_t<T>, Color4f>;
