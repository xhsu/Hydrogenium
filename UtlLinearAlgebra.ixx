/*
* Note from Luna:
* NEVER create constructor list here:
*	T(T&& v) = default;
*	T& operator=(const T& s) = default;
*	T& operator=(T&& s) = default;
* 
* Doing this would cause failing usage of ANY std template.
* With error: 'no suitable constructor find'.
*/

module;

// C
#include <cassert>

//#include <range/v3/range.hpp>	// #UPDATE_AT_CPP23 swap to std::ranges::zip
//#include <range/v3/view.hpp>

export module UtlLinearAlgebra;

// C++
import <array>;
import <concepts>;
import <format>;
//import <iomanip>;
//import <iostream>;
import <limits>;
import <numbers>;
import <ranges>;

// Static math lib
import "gcem/include/gcem.hpp";

// Friendly modules.
import UtlArithmetic;
import UtlConcepts;

// Concepts for this module.
template <typename A, size_t X> concept ProperArray = requires(A array) { requires array.max_size() >= X; };

export using vec_t = float;
export using real_t = std::conditional_t<sizeof(vec_t) <= 4, double, vec_t>;
export inline constexpr auto VEC_EPSILON = std::numeric_limits<vec_t>::epsilon();
export inline constexpr auto VEC_NAN = std::numeric_limits<vec_t>::quiet_NaN();
export inline constexpr auto VEC_INFINITY = std::numeric_limits<vec_t>::infinity();

// Used for many pathfinding and many other operations that are treated as planar rather than 3D.
export struct Vector2D
{
	// Construction
	constexpr Vector2D() noexcept : x{}, y{} {}
	constexpr Vector2D(real_t X, real_t Y) noexcept : x(static_cast<vec_t>(X)), y(static_cast<vec_t>(Y)) {}
	explicit constexpr Vector2D(real_t sideLength) noexcept : width(static_cast<vec_t>(sideLength)), height(static_cast<vec_t>(sideLength)) {}
	template <Arithmetic T, std::size_t _Size> requires(_Size >= 2U) explicit constexpr Vector2D(const T (&rgfl)[_Size]) noexcept : x(static_cast<vec_t>(rgfl[0])), y(static_cast<vec_t>(rgfl[1])) {}	// #UPDATE_AT_CPP23 could be fix in cpp23. Problem of comment: non-constexpr involved.
	template <Arithmetic T, std::size_t _Size> requires(_Size >= 2U) explicit constexpr Vector2D(const std::array<T, _Size>& rgfl) noexcept : x(static_cast<vec_t>(rgfl[0])), y(static_cast<vec_t>(rgfl[1])) {}
	explicit constexpr Vector2D(std::ranges::range auto&& RangeObj) noexcept : x{}, y{} { for (auto&& [Ref, Val] : ::ranges::views::zip(*this, RangeObj)) Ref = static_cast<vec_t>(Val); }	// #UPDATE_AT_CPP26 ranges::enumerator

	// Operators
	constexpr decltype(auto) operator-() const noexcept { return Vector2D(-x, -y); }
	constexpr bool operator==(const Vector2D& v) const noexcept { return Approx(v); }
	constexpr decltype(auto) operator<=> (const Vector2D& v) const noexcept { return LengthSquared() <=> v.LengthSquared(); }
	constexpr decltype(auto) operator<=> (real_t fl) const noexcept { return LengthSquared() <=> (fl * fl); }

	consteval decltype(auto) operator=(std::nullptr_t) noexcept { return (*this = Zero()); }

	constexpr decltype(auto) operator+(const Vector2D& v) const noexcept { return Vector2D(x + v.x, y + v.y); }
	constexpr decltype(auto) operator-(const Vector2D& v) const noexcept { return Vector2D(x - v.x, y - v.y); }
	constexpr decltype(auto) operator+=(const Vector2D& v) noexcept { return (*this = *this + v); }
	constexpr decltype(auto) operator-=(const Vector2D& v) noexcept { return (*this = *this - v); }

	constexpr decltype(auto) operator*(real_t fl) const noexcept { return Vector2D(x * fl, y * fl); }
	constexpr decltype(auto) operator/(real_t fl) const noexcept { return Vector2D(x / fl, y / fl); }
	constexpr decltype(auto) operator*=(real_t fl) noexcept { return (*this = *this * fl); }
	constexpr decltype(auto) operator/=(real_t fl) noexcept { return (*this = *this / fl); }

	// Static methods
	static consteval Vector2D Zero() noexcept { return Vector2D(0, 0); }
	static consteval Vector2D I() noexcept { return Vector2D(1, 0); }
	static consteval Vector2D J() noexcept { return Vector2D(0, 1); }

	// Methods
	constexpr void Clear() noexcept { x = 0; y = 0; }
	constexpr void CopyToIter(ProperIter auto it) const noexcept { using T = std::decay_t<decltype(*it)>; *it++ = static_cast<T>(x); *it++ = static_cast<T>(y); }
	constexpr void CopyToArray(ProperArray<2> auto& arr) const noexcept { using T = std::decay_t<decltype(arr[std::declval<size_t>()])>; arr[0] = static_cast<T>(x); arr[1] = static_cast<T>(y); }
	constexpr real_t Length() const noexcept { return Hydrogenium::sqrt(x * x + y * y); }	// Get the vector's magnitude
	constexpr real_t LengthSquared() const noexcept { return x * x + y * y; }	// Get the vector's magnitude squared
	constexpr Vector2D Normalize() const noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return Zero();

		const auto len = Length();
		return Vector2D(x / len, y / len);
	}
	constexpr real_t NormalizeInPlace() noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return (real_t)0;

		const auto len = Length();

		x = vec_t(x / len);
		y = vec_t(y / len);

		return len;
	}
	constexpr Vector2D SetLength(real_t flNewLength) const noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return Zero();

		const auto fl = flNewLength / Length();
		return Vector2D(x * fl, y * fl);
	}
	constexpr void SetLengthInPlace(real_t flNewLength) noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return;

		const auto fl = flNewLength / Length();

		x = static_cast<vec_t>(x * fl);
		y = static_cast<vec_t>(y * fl);
	}
	constexpr bool IsZero(vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return (
			x > -tolerance && x < tolerance &&
			y > -tolerance && y < tolerance
		);
	}
	constexpr bool IsNaN() const noexcept { return Hydrogenium::is_nan(x, y); }
	constexpr bool Approx(const Vector2D& rhs, vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return
			gcem::abs(x - rhs.x) < tolerance &&
			gcem::abs(y - rhs.y) < tolerance;
	}

	// Conversion
	constexpr operator vec_t* () noexcept { return &x; } // Vectors will now automatically convert to float * when needed
	constexpr operator const vec_t* () const noexcept { return &x; } // Vectors will now automatically convert to float * when needed

	explicit constexpr operator bool () const noexcept { return !IsZero(); }	// Can be placed in if() now.
	explicit constexpr operator vec_t() const noexcept { return (vec_t)Length(); }
	explicit constexpr operator real_t() const noexcept { return Length(); }

	// Linear Algebra
	// Rotate in counter-clockwise. Angle is in degree.
	constexpr Vector2D Rotate(real_t angle) const noexcept
	{
		const auto a = (angle * std::numbers::pi / 180.0);
		const auto c = gcem::cos(a);
		const auto s = gcem::sin(a);

		return Vector2D(
			c * x - s * y,
			s * x + c * y
		);
	}

	// Angle with Vector2D::I. Return in degree.
	constexpr auto Angle(void) const noexcept
	{
		return gcem::atan2<real_t, real_t>(y, x) * 180.0 / std::numbers::pi;
	}

	// STL Containers Compatibility
	// Iterators
	using value_type = vec_t;
	using iterator = _STD _Array_iterator<value_type, 2>;
	using const_iterator = _STD _Array_const_iterator<value_type, 2>;
	using reverse_iterator = _STD reverse_iterator<iterator>;
	using const_reverse_iterator = _STD reverse_iterator<const_iterator>;
	[[nodiscard]] constexpr iterator begin(void) noexcept { return iterator(&x, 0); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator begin(void) const noexcept { return const_iterator(&x, 0); }
	[[nodiscard]] constexpr iterator end(void) noexcept { return iterator(&x, 2); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator end(void) const noexcept { return const_iterator(&x, 2); }
	[[nodiscard]] constexpr reverse_iterator rbegin(void) noexcept { return reverse_iterator(end()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rbegin(void) const noexcept { return const_reverse_iterator(end()); }
	[[nodiscard]] constexpr reverse_iterator rend(void) noexcept { return reverse_iterator(begin()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rend(void) const noexcept { return const_reverse_iterator(begin()); }
	[[nodiscard]] constexpr const_iterator cbegin(void) const noexcept { return begin(); }
	[[nodiscard]] constexpr const_iterator cend(void) const noexcept { return end(); }
	[[nodiscard]] constexpr const_reverse_iterator crbegin(void) const noexcept { return rbegin(); }
	[[nodiscard]] constexpr const_reverse_iterator crend(void) const noexcept { return rend(); }

	// Element Access
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;
	[[nodiscard]] constexpr reference at(size_type pos)
	{
		switch (pos)
		{
		case 0:
			return x;
		case 1:
			return y;
		[[unlikely]] default:
			throw std::out_of_range(std::format("[Vector2D::at] Invalid accessing pos: {}.", pos));
		}
	}
	[[nodiscard]] constexpr const_reference at(size_type pos) const	// #UPDATE_AT_CPP23 explict this
	{
		switch (pos)
		{
		case 0:
			return x;
		case 1:
			return y;
		[[unlikely]] default:
			throw std::out_of_range(std::format("[Vector2D::at] Invalid accessing pos: {}.", pos));
		}
	}
	//[[nodiscard]] constexpr reference operator[] (std::size_t pos) noexcept { return *((&x) + pos); }
	//[[nodiscard]] constexpr const_reference operator[] (std::size_t pos) const noexcept { return *((&x) + pos); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr pointer data(void) noexcept { return &x; }
	[[nodiscard]] constexpr const_pointer data(void) const noexcept { return &x; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference front(void) noexcept { return x; }
	[[nodiscard]] constexpr const_reference front(void) const noexcept { return x; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference back(void) noexcept { return y; }
	[[nodiscard]] constexpr const_reference back(void) const noexcept { return y; }	// #UPDATE_AT_CPP23 explict this

	// Capacity
	[[nodiscard]] static consteval bool empty(void) noexcept { return false; }
	[[nodiscard]] static consteval size_type size(void) noexcept { return static_cast<size_type>(2); }
	[[nodiscard]] static consteval size_type max_size(void) noexcept { return static_cast<size_type>(2); }

	// Modifiers
	constexpr void fill(const_reference val) noexcept { x = y = val; }
	constexpr void swap(Vector2D& other) noexcept { _STD _Swap_ranges_unchecked(&x, (&x) + 2, &other.x); }

	// Members
	union { vec_t x; vec_t width; };
	union { vec_t y; vec_t height; };
};

export constexpr real_t DotProduct(const Vector2D& a, const Vector2D& b) noexcept
{
	return (a.x * b.x + a.y * b.y);
}

export constexpr Vector2D operator*(real_t fl, const Vector2D& v) noexcept
{
	return v * fl;
}

export constexpr decltype(auto) operator^(const Vector2D& lhs, const Vector2D& rhs) noexcept	// Get the angle between two vectors. Returns an angle in degree.
{
	const real_t length_ab = gcem::sqrt(lhs.LengthSquared() * rhs.LengthSquared());	// sqrt(a) * sqrt(b) == sqrt(a*b)

	if (length_ab < std::numeric_limits<real_t>::epsilon())
		return (real_t)0;

	return gcem::acos(DotProduct(lhs, rhs) / length_ab) * (180.0 / std::numbers::pi);
}

#ifdef _IOSTREAM_
export std::ostream& operator<<(std::ostream& o, const Vector2D& v) noexcept
{
	o << "X: " << v.x << '\n';
	o << "Y: " << v.y << '\n';
	return o;
}
#endif // _IOSTREAM_

// 3D Vector
// Same data-layout as engine's vec3_t, which is a vec_t[3]
export struct Vector
{
	// Construction
	constexpr Vector() noexcept : x{}, y{}, z{} {}
	constexpr Vector(real_t X, real_t Y, real_t Z) noexcept : x(static_cast<vec_t>(X)), y(static_cast<vec_t>(Y)), z(static_cast<vec_t>(Z)) {}
	constexpr Vector(const Vector2D& v2d, real_t Z) noexcept : x(v2d.x), y(v2d.y), z(static_cast<vec_t>(Z)) {}
	template <Arithmetic T, std::size_t _Size> requires(_Size >= 3U) explicit constexpr Vector(const T(&rgfl)[_Size]) noexcept : x(static_cast<vec_t>(rgfl[0])), y(static_cast<vec_t>(rgfl[1])), z(static_cast<vec_t>(rgfl[2])) {}
	template <Arithmetic T, std::size_t _Size> requires(_Size >= 3U) explicit constexpr Vector(const std::array<T, _Size>& rgfl) noexcept : x(static_cast<vec_t>(rgfl[0])), y(static_cast<vec_t>(rgfl[1])), z(static_cast<vec_t>(rgfl[2])) {}
	explicit constexpr Vector(std::ranges::range auto&& RangeObj) noexcept : x{}, y{}, z{} { for (auto&& [Ref, Val] : ::ranges::views::zip(*this, RangeObj)) Ref = static_cast<vec_t>(Val); }	// #UPDATE_AT_CPP26 ranges::enumerator

	// Operators
	constexpr decltype(auto) operator-() const noexcept { return Vector(-x, -y, -z); }
	constexpr bool operator==(const Vector& v) const noexcept { return Approx(v); }
	constexpr decltype(auto) operator<=> (const Vector& v) const noexcept { return LengthSquared() <=> v.LengthSquared(); }
	constexpr decltype(auto) operator<=> (real_t fl) const noexcept { return LengthSquared() <=> (fl * fl); }

	consteval decltype(auto) operator=(std::nullptr_t) noexcept { return (*this = Zero()); }

	constexpr decltype(auto) operator+(const Vector& v) const noexcept { return Vector(x + v.x, y + v.y, z + v.z); }
	constexpr decltype(auto) operator-(const Vector& v) const noexcept { return Vector(x - v.x, y - v.y, z - v.z); }
	constexpr decltype(auto) operator+=(const Vector& v) noexcept { return (*this = *this + v); }
	constexpr decltype(auto) operator-=(const Vector& v) noexcept { return (*this = *this - v); }

	constexpr decltype(auto) operator*(real_t fl) const noexcept { return Vector(x * fl, y * fl, z * fl); }
	constexpr decltype(auto) operator/(real_t fl) const noexcept { return Vector(x / fl, y / fl, z / fl); }
	constexpr decltype(auto) operator*=(real_t fl) noexcept { return (*this = *this * fl); }
	constexpr decltype(auto) operator/=(real_t fl) noexcept { return (*this = *this / fl); }

	// Static methods
	static consteval Vector Zero() noexcept { return Vector(0, 0, 0); }
	static consteval Vector I() noexcept { return Vector(1, 0, 0); }
	static consteval Vector J() noexcept { return Vector(0, 1, 0); }
	static consteval Vector K() noexcept { return Vector(0, 0, 1); }

	// Methods
	constexpr void Clear() noexcept { x = y = z = 0; }
	constexpr void CopyToIter(ProperIter auto it) const noexcept { using T = std::decay_t<decltype(*it)>; *it++ = static_cast<T>(x); *it++ = static_cast<T>(y); *it++ = static_cast<T>(z); }
	constexpr void CopyToArray(ProperArray<3> auto& arr) const noexcept { using T = std::decay_t<decltype(arr[std::declval<size_t>()])>; arr[0] = static_cast<T>(x); arr[1] = static_cast<T>(y); arr[2] = static_cast<T>(z); }
	constexpr real_t Length() const noexcept { return Hydrogenium::sqrt<real_t>(x * x + y * y + z * z); }	// Get the vector's magnitude
	constexpr real_t LengthSquared() const noexcept { return (x * x + y * y + z * z); }	// Get the vector's magnitude squared
	constexpr real_t Length2D() const noexcept { return Hydrogenium::sqrt(x * x + y * y); }	// Get the vector's magnitude, but only consider its X and Y component
	constexpr real_t Length2DSquared() const noexcept { return (x * x + y * y); }
	constexpr Vector Normalize() const noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return Zero();

		const auto len = Length();
		return Vector(x / len, y / len, z / len);
	}
	constexpr real_t NormalizeInPlace() noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return (real_t)0;

		const auto len = Length();

		x = vec_t(x / len);
		y = vec_t(y / len);
		z = vec_t(z / len);

		return len;
	}
	constexpr Vector SetLength(real_t flNewLength) const noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return Zero();

		const auto fl = flNewLength / Length();
		return Vector(x * fl, y * fl, z * fl);
	}
	constexpr void SetLengthInPlace(real_t flNewLength) noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return;

		const auto fl = flNewLength / Length();

		x = static_cast<vec_t>(x * fl);
		y = static_cast<vec_t>(y * fl);
		z = static_cast<vec_t>(z * fl);
	}
	constexpr bool IsZero(vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return (
			x > -tolerance && x < tolerance &&
			y > -tolerance && y < tolerance &&
			z > -tolerance && z < tolerance
		);
	}
	constexpr bool IsNaN() const noexcept { return Hydrogenium::is_nan(x, y, z); }
	constexpr Vector2D Make2D() const noexcept { return Vector2D(x, y); }
	constexpr bool Approx(const Vector& rhs, vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return
			gcem::abs(x - rhs.x) < tolerance &&
			gcem::abs(y - rhs.y) < tolerance &&
			gcem::abs(z - rhs.z) < tolerance;
	}

	// Conversion
	constexpr operator vec_t* () noexcept { return &x; } // Vectors will now automatically convert to float * when needed
	constexpr operator const vec_t* () const noexcept { return &x; } // Vectors will now automatically convert to float * when needed
	
	explicit constexpr operator bool() const noexcept { return !IsZero(); }	// Can be placed in if() now.
	explicit constexpr operator vec_t() const noexcept { return (vec_t)Length(); }
	explicit constexpr operator real_t() const noexcept { return Length(); }

	// Linear Algebra

	constexpr Vector Forward() const noexcept
	{
		const auto sp = gcem::sin(rad_pitch), sy = gcem::sin(rad_yaw);
		const auto cp = gcem::cos(rad_pitch), cy = gcem::cos(rad_yaw);

		return Vector(
			cp * cy,	// x
			cp * sy,	// y
			-sp		// z
		);
	}
	constexpr Vector Right() const noexcept
	{
		const auto sp = gcem::sin(rad_pitch), sy = gcem::sin(rad_yaw), sr = gcem::sin(rad_roll);
		const auto cp = gcem::cos(rad_pitch), cy = gcem::cos(rad_yaw), cr = gcem::cos(rad_roll);

		return Vector(
			-(sr * sp * cy) + cr * sy,	// x
			-(sr * sp * sy) - cr * cy,	// y
			-(sr * cp)		// z
		);
	}
	constexpr Vector Up() const noexcept
	{
		const auto sp = gcem::sin(rad_pitch), sy = gcem::sin(rad_yaw), sr = gcem::sin(rad_roll);
		const auto cp = gcem::cos(rad_pitch), cy = gcem::cos(rad_yaw), cr = gcem::cos(rad_roll);

		return Vector(
			cr * sp * cy + sr * sy,	// x
			cr * sp * sy - sr * cy,	// y
			cr * cp		// z
		);
	}

	// Dismantle an set of Eular angles to three vectors.
	constexpr std::tuple<Vector, Vector, Vector> AngleVectors() const noexcept
	{
		const auto sp = gcem::sin(rad_pitch), sy = gcem::sin(rad_yaw), sr = gcem::sin(rad_roll);
		const auto cp = gcem::cos(rad_pitch), cy = gcem::cos(rad_yaw), cr = gcem::cos(rad_roll);

		return std::make_tuple(

			// Forward
			Vector(
				cp * cy,	// x
				cp * sy,	// y
				-sp		// z
			),

			// Right
			Vector(
				-(sr * sp * cy) + cr * sy,	// x
				-(sr * sp * sy) - cr * cy,	// y
				-(sr * cp)		// z
			),

			// Up
			Vector(
				cr * sp * cy + sr * sy,	// x
				cr * sp * sy - sr * cy,	// y
				cr * cp		// z
			)
		);
	}

	// Convert an forward vector to a set of Eular angles. Note: The ROLL into ALWAYS lost in the process.
	constexpr Vector VectorAngles(void) const noexcept
	{
		Vector angles;

		if (y == 0 && x == 0)
		{
			angles.yaw = 0;
			if (z > 0)
				angles.pitch = 90;
			else
				angles.pitch = 270;
		}
		else
		{
			angles.rad_yaw = gcem::atan2<real_t>(y, x);
			if (angles.yaw < 0)
				angles.yaw += 360;

			const auto tmp = gcem::sqrt<real_t>(x * x + y * y);
			angles.rad_pitch = gcem::atan2<real_t>(z, tmp);
			if (angles.pitch < 0)
				angles.pitch += 360;
		}

		return angles;
	}

	// Unify three vectors into one Eular angles.
	static constexpr Vector VectorsAngles(const Vector& vecForward, const Vector& vecRight, const Vector& vecUp) noexcept
	{
		Vector ret;
		const auto p = -gcem::asin<real_t>(vecForward.z);
		auto cp = gcem::cos(p);

		if (gcem::abs(cp) > VEC_EPSILON)	// gimball lock?
		{
			cp = 1.0 / cp;
			ret.rad_pitch = p;
			ret.rad_yaw = gcem::atan2<real_t>(vecForward.y * cp, vecForward.x * cp);
			ret.rad_roll = gcem::atan2<real_t>(-vecRight.z * cp, vecUp.z * cp);
		}
		else
		{
			ret.pitch = (vec_t)gcem::copysign(90, vecForward.z);
			ret.rad_yaw = gcem::atan2<real_t>(vecRight.x, -vecRight.y);
			ret.roll = 180.0f;
		}

		return ret;
	}

	constexpr Vector RotateX(real_t angle) const noexcept
	{
		const auto a = (angle * std::numbers::pi / 180.0);
		const auto c = gcem::cos(a);
		const auto s = gcem::sin(a);

		return Vector(
			x,
			c * y - s * z,
			s * y + c * z
		);
	}
	constexpr Vector RotateY(real_t angle) const noexcept
	{
		const auto a = (angle * std::numbers::pi / 180.0);
		const auto c = gcem::cos(a);
		const auto s = gcem::sin(a);

		return Vector(
			c * x + s * z,
			y,
			-s * x + c * z
		);
	}
	constexpr Vector RotateZ(real_t angle) const noexcept
	{
		const auto a = (angle * std::numbers::pi / 180.0);
		const auto c = gcem::cos(a);
		const auto s = gcem::sin(a);

		return Vector(
			c * x - s * y,
			s * x + c * y,
			z
		);
	}

	// Euler
	constexpr vec_t _impl_deg_pitch_get() const noexcept { return x; }
	constexpr void _impl_deg_pitch_put(vec_t P) noexcept { x = P; }
	constexpr vec_t _impl_deg_yaw_get() const noexcept { return y; }
	constexpr void _impl_deg_yaw_put(vec_t Y) noexcept { y = Y; }
	constexpr vec_t _impl_deg_roll_get() const noexcept { return z; }
	constexpr void _impl_deg_roll_put(vec_t R) noexcept { z = R; }
	constexpr real_t _impl_rad_pitch_get() const noexcept { return x * std::numbers::pi / 180.0; }
	constexpr void _impl_rad_pitch_put(real_t P) noexcept { x = static_cast<vec_t>(P * std::numbers::inv_pi * 180.0); }
	constexpr real_t _impl_rad_yaw_get() const noexcept { return y * std::numbers::pi / 180.0; }
	constexpr void _impl_rad_yaw_put(real_t Y) noexcept { y = static_cast<vec_t>(Y * std::numbers::inv_pi * 180.0); }
	constexpr real_t _impl_rad_roll_get() const noexcept { return z * std::numbers::pi / 180.0; }
	constexpr void _impl_rad_roll_put(real_t R) noexcept { z = static_cast<vec_t>(R * std::numbers::inv_pi * 180.0); }

	// STL Containers Compatibility
	// Iterators
	using value_type = vec_t;
	using iterator = _STD _Array_iterator<value_type, 3>;
	using const_iterator = _STD _Array_const_iterator<value_type, 3>;
	using reverse_iterator = _STD reverse_iterator<iterator>;
	using const_reverse_iterator = _STD reverse_iterator<const_iterator>;
	[[nodiscard]] constexpr iterator begin(void) noexcept { return iterator(&x, 0); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator begin(void) const noexcept { return const_iterator(&x, 0); }
	[[nodiscard]] constexpr iterator end(void) noexcept { return iterator(&x, 3); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator end(void) const noexcept { return const_iterator(&x, 3); }
	[[nodiscard]] constexpr reverse_iterator rbegin(void) noexcept { return reverse_iterator(end()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rbegin(void) const noexcept { return const_reverse_iterator(end()); }
	[[nodiscard]] constexpr reverse_iterator rend(void) noexcept { return reverse_iterator(begin()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rend(void) const noexcept { return const_reverse_iterator(begin()); }
	[[nodiscard]] constexpr const_iterator cbegin(void) const noexcept { return begin(); }
	[[nodiscard]] constexpr const_iterator cend(void) const noexcept { return end(); }
	[[nodiscard]] constexpr const_reverse_iterator crbegin(void) const noexcept { return rbegin(); }
	[[nodiscard]] constexpr const_reverse_iterator crend(void) const noexcept { return rend(); }

	// Element Access
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;
	[[nodiscard]] constexpr reference at(size_type pos)
	{
		switch (pos)
		{
		case 0:
			return x;
		case 1:
			return y;
		case 2:
			return z;
		[[unlikely]] default:
			throw std::out_of_range(std::format("[Vector::at] Invalid accessing pos: {}.", pos));
		}
	}
	[[nodiscard]] constexpr const_reference at(size_type pos) const	// #UPDATE_AT_CPP23 explict this
	{
		switch (pos)
		{
		case 0:
			return x;
		case 1:
			return y;
		case 2:
			return z;
		[[unlikely]] default:
			throw std::out_of_range(std::format("[Vector::at] Invalid accessing pos: {}.", pos));
		}
	}
	//[[nodiscard]] constexpr reference operator[] (std::size_t pos) noexcept { return *((&x) + pos); }
	//[[nodiscard]] constexpr const_reference operator[] (std::size_t pos) const noexcept { return *((&x) + pos); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr pointer data(void) noexcept { return &x; }
	[[nodiscard]] constexpr const_pointer data(void) const noexcept { return &x; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference front(void) noexcept { return x; }
	[[nodiscard]] constexpr const_reference front(void) const noexcept { return x; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference back(void) noexcept { return z; }
	[[nodiscard]] constexpr const_reference back(void) const noexcept { return z; }	// #UPDATE_AT_CPP23 explict this

	// Capacity
	[[nodiscard]] static consteval bool empty(void) noexcept { return false; }
	[[nodiscard]] static consteval size_type size(void) noexcept { return static_cast<size_type>(3); }
	[[nodiscard]] static consteval size_type max_size(void) noexcept { return static_cast<size_type>(3); }

	// Modifiers
	constexpr void fill(const_reference val) noexcept { x = y = z = val; }
	constexpr void swap(Vector& other) noexcept { _STD _Swap_ranges_unchecked(&x, (&x) + 3, &other.x); }

	// Members
	vec_t x { 0 };
	vec_t y { 0 };
	vec_t z { 0 };
	__declspec(property(get = _impl_deg_pitch_get, put = _impl_deg_pitch_put)) vec_t pitch;
	__declspec(property(get = _impl_rad_pitch_get, put = _impl_rad_pitch_put)) real_t rad_pitch;
	__declspec(property(get = _impl_deg_yaw_get, put = _impl_deg_yaw_put)) vec_t yaw;
	__declspec(property(get = _impl_rad_yaw_get, put = _impl_rad_yaw_put)) real_t rad_yaw;
	__declspec(property(get = _impl_deg_roll_get, put = _impl_deg_roll_put)) vec_t roll;
	__declspec(property(get = _impl_rad_roll_get, put = _impl_rad_roll_put)) real_t rad_roll;
};

export constexpr Vector operator*(real_t fl, const Vector& v) noexcept
{
	return v * fl;
}

export constexpr real_t DotProduct(const Vector& a, const Vector& b) noexcept
{
	return (a.x * b.x + a.y * b.y + a.z * b.z);
}

export constexpr real_t DotProduct2D(const Vector& a, const Vector& b) noexcept
{
	return (a.x * b.x + a.y * b.y);
}

export constexpr Vector CrossProduct(const Vector& a, const Vector& b) noexcept
{
	return Vector(
		a.y * b.z - a.z * b.y,
		a.z * b.x - a.x * b.z,
		a.x * b.y - a.y * b.x
	);
}

export constexpr Vector CrossProduct(const Vector2D& a, const Vector2D& b) noexcept
{
	return Vector(
		0,
		0,
		a.x * b.y - a.y * b.x
	);
}

// Get the angle between two vectors. Returns an angle in degree.
export constexpr decltype(auto) operator^(const Vector& a, const Vector& b) noexcept
{
	const real_t length_ab = gcem::sqrt(a.LengthSquared() * b.LengthSquared());	// sqrt(a) * sqrt(b) == sqrt(a*b)

	if (length_ab < std::numeric_limits<real_t>::epsilon())
		return (real_t)0;

	return gcem::acos(DotProduct(a, b) / length_ab) * (180.0 / std::numbers::pi);
}

#ifdef _IOSTREAM_
export std::ostream& operator<<(std::ostream& o, const Vector& v) noexcept
{
	o << "X: " << v.x << std::endl;
	o << "Y: " << v.y << std::endl;
	o << "Z: " << v.z << std::endl;
	return o;
}
#endif // _IOSTREAM_

export using mxs_t = double;
export inline constexpr auto MXS_EPSILON = std::numeric_limits<mxs_t>::epsilon();
export inline constexpr auto MXS_NAN = std::numeric_limits<mxs_t>::quiet_NaN();
export inline constexpr auto MXS_INFINITY = std::numeric_limits<mxs_t>::infinity();

export template <size_t _rows, size_t _cols>
requires(_rows > 0U && _cols > 0U)
struct Matrix
{
	// Constants
	static constexpr auto ROWS = _rows;
	static constexpr auto COLUMNS = _cols;
	static constexpr bool SQUARE_MX = _rows == _cols;
	static constexpr auto RxC = _rows * _cols;
	static constexpr auto DIAGONAL = (size_t)gcem::sqrt(RxC);

	// Types
	using RowInit_t = std::initializer_list<mxs_t>;
	using This_t = Matrix<ROWS, COLUMNS>;

	// Constructors
	constexpr Matrix() noexcept : _data{} {}
	template <Arithmetic T> constexpr Matrix(const T(&array)[ROWS][COLUMNS]) noexcept	// Why can't I use the keyword 'auto' as auto-template here?
	{
		[&] <size_t... I>(std::index_sequence<I...>&&) constexpr
		{
			if constexpr (ROWS == 1)
				((_data[0][I] = array[0][I]), ...);
			else if constexpr (COLUMNS == 1)
				((_data[I][0] = array[I][0]), ...);
			else
				((_data[I / ROWS][I % COLUMNS] = array[I / ROWS][I % COLUMNS]), ...);
		}
		(std::make_index_sequence<RxC>{});
	}
	constexpr Matrix(const std::initializer_list<RowInit_t>&& list) noexcept
	{
		assert(list.size() >= ROWS);
		size_t r = 0;

		for (auto& row : list)
		{
			assert(row.size() >= COLUMNS);
			size_t c = 0;

			for (auto& cell : row)
			{
				_data[r][c] = cell;
				c++;
			}

			r++;
		}
	}
	constexpr Matrix(Arithmetic auto... cells) noexcept
	{
		static_assert(sizeof...(cells) == RxC, "%RxC% arguments must be provided.");

		[&] <size_t... I>(std::index_sequence<I...>&&) constexpr
		{
			if constexpr (ROWS == 1)
				((_data[0][I] = cells), ...);
			else if constexpr (COLUMNS == 1)
				((_data[I][0] = cells), ...);
			else
				((_data[I / ROWS][I % COLUMNS] = cells), ...);	// Can only be used when COLUMNS > 1. Otherwise I % 1 would always return 0 hence failing the pack extensions.
		}
		(std::make_index_sequence<RxC>{});
	}
	constexpr Matrix(const RowInit_t&& list) noexcept
	{
		assert(list.size() >= RxC);

		auto iter = list.begin();
		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				assert(iter != list.end());	// list is too short!

				_data[i][j] = *iter;
				++iter;
			}
		}
	}
	template <size_t BRows, size_t BCols> explicit constexpr Matrix(const Matrix<BRows, BCols>& B) noexcept : _data{}	// Enforce conversion.
	{
		if constexpr (SQUARE_MX)
		{
			*this = Identity();
		}

		constexpr size_t R = std::min(BRows, ROWS);
		constexpr size_t C = std::min(BCols, COLUMNS);

		[&] <size_t... I>(std::index_sequence<I...>&&)
		{
			((_data[I / R][I % C] = B[I / R][I % C]), ...);
		}
		(std::make_index_sequence<R * C>{});
	}
	explicit constexpr Matrix(const Vector2D &v) noexcept requires(ROWS >= 2U && COLUMNS == 1U) : _data{}
	{
		_data[0][0] = v.x;
		_data[1][0] = v.y;

		if constexpr (ROWS > 2U)
		{
			_data[ROWS - 1U][0] = 1;	// For example, if you wish Vector2(x, y) transcript to matrix<4, 1>, it must be [x, y, 0, 1].
		}
	}
	explicit constexpr Matrix(const Vector &v) noexcept requires(ROWS >= 3U && COLUMNS == 1U) : _data{}
	{
		_data[0][0] = v.x;
		_data[1][0] = v.y;
		_data[2][0] = v.z;

		if constexpr (ROWS > 3U)
		{
			_data[ROWS - 1U][0] = 1;	// For example, if you wish Vector3(x, y, z) transcript to matrix<5, 1>, it must be [x, y, z, 0, 1].
		}
	}

	// Static Methods
	static consteval decltype(auto) Identity() noexcept requires(SQUARE_MX)
	{
		return [] <size_t... I>(std::index_sequence<I...>&&) -> This_t
		{
			return This_t(
				!(bool)(I % (ROWS + 1))...
			);
		}
		(std::make_index_sequence<RxC>{});
	}
	static consteval decltype(auto) Zero() noexcept { return This_t(); }
	static constexpr decltype(auto) Rotation(mxs_t degree) noexcept	// 2D rotation. Ideally generates a 2x2 matrix.
	{
		const auto rad = degree / 180.0 * std::numbers::pi;
		const auto c = gcem::cos(rad);
		const auto s = gcem::sin(rad);

		if constexpr (ROWS == 2U && COLUMNS == 2U)
		{
			return Matrix<2, 2>({
				{c, -s},
				{s, c}
			});
		}
		else
		{
			return static_cast<This_t>(	// Use our special defined matrix convert function.
				Matrix<2, 2>({
					{c, -s},
					{s, c}
				})
			);
		}
	}
	static constexpr decltype(auto) Rotation(mxs_t yaw, mxs_t pitch, mxs_t roll) noexcept // 3D rotation. yaw (Z), pitch (Y), roll (X)
	{
		const auto y = yaw / 180.0 * std::numbers::pi, p = pitch / 180.0 * std::numbers::pi, r = roll / 180.0 * std::numbers::pi;
		const auto cy = gcem::cos(y), sy = gcem::sin(y);
		const auto cp = gcem::cos(p), sp = gcem::sin(p);
		const auto cr = gcem::cos(r), sr = gcem::sin(r);

		if constexpr (ROWS == 3U && COLUMNS == 3U)
		{
			return This_t({
				{cy * cp, cy * sp * sr - sy * cr, cy * sp * cr + sy * sr},
				{sy * cp, sy * sp * sr + cy * cr, sy * sp * cr - cy * sr},
				{-sp, cp * sr, cp * cr}
			});
		}
		else
		{
			return static_cast<This_t>(Matrix<3, 3>({
				{cy * cp, cy * sp * sr - sy * cr, cy * sp * cr + sy * sr},
				{sy * cp, sy * sp * sr + cy * cr, sy * sp * cr - cy * sr},
				{-sp, cp * sr, cp * cr}
			}));
		}
	}
	static constexpr decltype(auto) Rotation(const Vector& vecEulerAngles) noexcept { return Rotation(vecEulerAngles.yaw, vecEulerAngles.pitch, vecEulerAngles.roll); }
	static constexpr decltype(auto) Rotation(const Vector& vecAxis, double degree) noexcept	// Axis must be a unit vector. In counter-clockwise. Quaternion is recommended in this case.
	{
		const auto& x = vecAxis.x;
		const auto& y = vecAxis.y;
		const auto& z = vecAxis.z;

		degree *= std::numbers::pi / 180.0;
		const auto c = gcem::cos(degree);
		const auto s = gcem::sin(degree);

		if constexpr (ROWS == 3U && COLUMNS == 3U)
		{
			return This_t({
				{c + x * x * (1 - c), x * y * (1 - c) - z * s, x * z * (1 - c) + y * s},
				{y * x * (1 - c) + z * s, c + y * y * (1 - c), y * z * (1 - c) - x * s},
				{z * x * (1 - c) - y * s, z * y * (1 - c) + x * s, c + z * z * (1 - c)}
			});
		}
		else
		{
			return static_cast<This_t>(Matrix<3, 3>({
				{c + x * x * (1 - c), x * y * (1 - c) - z * s, x * z * (1 - c) + y * s},
				{y * x * (1 - c) + z * s, c + y * y * (1 - c), y * z * (1 - c) - x * s},
				{z * x * (1 - c) - y * s, z * y * (1 - c) + x * s, c + z * z * (1 - c)}
			}));
		}
	}
	static constexpr decltype(auto) Scale(Arithmetic auto&&... scale) noexcept requires(SQUARE_MX && sizeof...(scale) == DIAGONAL)
	{
		return [scale...] <size_t... I>(std::index_sequence<I...>&&) { This_t m; ((m[I][I] = static_cast<mxs_t>(scale)), ...); return m; }(std::make_index_sequence<DIAGONAL>{});
	}
	static constexpr decltype(auto) Translate(Arithmetic auto&&... deltas) noexcept requires(COLUMNS == ROWS + 1 || SQUARE_MX)
	{
		static_assert(sizeof...(deltas) == ROWS - SQUARE_MX,
			"The amount of arguments must be exactly as rows (with squared matrix) or one less (with things like matrix 4x3)!"
		);

		return [deltas...] <size_t... I>(std::index_sequence<I...>&&)
		{
			This_t m = Identity();
			((m[I][COLUMNS - 1] = deltas), ...);
			return m;
		}
		(std::make_index_sequence<ROWS - SQUARE_MX>{});
	}

	// Properties
	constexpr decltype(auto) Transpose() const noexcept
	{
		Matrix<COLUMNS, ROWS> m;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				m[j][i] = _data[i][j];
			}
		}

		return m;
	}
	constexpr decltype(auto) Cofactor(size_t r, size_t c) const noexcept requires(ROWS > 1U && COLUMNS > 1U)
	{
		assert(r < ROWS);
		assert(c < COLUMNS);

		Matrix<ROWS - 1U, COLUMNS - 1U> m;

		for (size_t i = 0; i < ROWS; i++)
		{
			size_t row = 0U;
			if (i < r)
				row = i;
			else if (i > r)
				row = i - 1U;
			else // i == r, same row.
				continue;

			for (size_t j = 0; j < COLUMNS; j++)
			{
				size_t col = 0U;
				if (j < c)
					col = j;
				else if (j > c)
					col = j - 1U;
				else // j == c, same column.
					continue;

				m[row][col] = _data[i][j];
			}
		}

		return m;
	}
	constexpr decltype(auto) Cofactor() const noexcept requires(SQUARE_MX)
	{
		This_t m;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				m[i][j] = (((i + j) % 2U == 0U) ? 1.0 : -1.0) * Cofactor(i, j).Determinant();
			}
		}

		return m;
	}
	constexpr decltype(auto) Determinant() const noexcept requires(SQUARE_MX)
	{
		// Base case: if matrix contains single element
		if constexpr (ROWS == 1U)
		{
			// The usage of STATIC_IF here is because that Matrix<0, 0> will cause error.
			return _data[0][0];
		}
		else
		{
			mxs_t D = 0; // Initialize result
			float sign = 1;	// To store sign multiplier

			// Iterate for each element of first row
			for (size_t f = 0; f < COLUMNS; f++)
			{
				// Getting Cofactor of A[0][f]
				D += sign * _data[0][f] * Cofactor(0, f).Determinant();

				// terms are to be added with alternate sign
				sign *= -1.0f;
			}

			return D;
		}
	}
	constexpr decltype(auto) Adjoint() const noexcept requires(SQUARE_MX)
	{
		if constexpr (ROWS == 1U && COLUMNS == 1U)
		{
			static const Matrix<1, 1> m({ {1.0} });
			return m;
		}
		else
		{
			This_t m;

			for (size_t i = 0; i < ROWS; i++)
			{
				for (size_t j = 0; j < COLUMNS; j++)
				{
					// Transpose of the cofactor matrix.
					m[j][i] = (((i + j) % 2U == 0U) ? 1.0 : -1.0) * Cofactor(i, j).Determinant();
				}
			}

			return m;
		}
	}
	constexpr decltype(auto) Inverse() const noexcept requires(SQUARE_MX)
	{
		auto det = Determinant();
		assert(det != 0);	// Singular matrices have no inverse.

		return Adjoint() / det;
	}

	// Methods
	constexpr void ReplaceCol(size_t c, Arithmetic auto... vals) noexcept
	{
		static_assert(sizeof...(vals) == ROWS, "You must provide %ROWS% arguments.");
		assert(c < COLUMNS);

		[&] <size_t... I>(std::index_sequence<I...>&&)
		{
			((_data[I][c] = vals), ...);
		}
		(std::make_index_sequence<ROWS>{});
	}
	constexpr void ReplaceRow(size_t r, Arithmetic auto... vals) noexcept
	{
		static_assert(sizeof...(vals) == ROWS, "You must provide %COLUMNS% arguments.");
		assert(r < ROWS);

		[&] <size_t... I>(std::index_sequence<I...>&&)
		{
			((_data[r][I] = vals), ...);
		}
		(std::make_index_sequence<COLUMNS>{});
	}
	constexpr bool IsZero(mxs_t tolerance = MXS_EPSILON) const noexcept { return Approx(Zero(), tolerance); }
	constexpr bool IsNaN() const noexcept
	{
		return [&] <size_t... I>(std::index_sequence<I...>&&) constexpr -> bool
		{
			if constexpr (ROWS == 1)
				return Hydrogenium::is_nan(_data[0][I]...);
			else if constexpr (COLUMNS == 1)
				return Hydrogenium::is_nan(_data[I][0]...);
			else
				return Hydrogenium::is_nan(_data[I / ROWS][I % COLUMNS]...);
		}
		(std::make_index_sequence<RxC>{});
	}
	constexpr bool Approx(const This_t& B, mxs_t tolerance = MXS_EPSILON) const noexcept
	{
		return [&] <size_t... I>(std::index_sequence<I...>&&) constexpr -> bool
		{
			if constexpr (ROWS == 1)
				return((gcem::abs(B[0][I] - _data[0][I]) < tolerance) && ...);
			else if constexpr (COLUMNS == 1)
				return((gcem::abs(B[I][0] - _data[I][0]) < tolerance) && ...);
			else
				return ((gcem::abs(B[I / ROWS][I % COLUMNS] - _data[I / ROWS][I % COLUMNS]) < tolerance) && ...);
		}
		(std::make_index_sequence<RxC>{});
	}

	// Operators
	// 
	// Between matrices.
	template <size_t BRows, size_t BCols> constexpr decltype(auto) operator==(const Matrix<BRows, BCols>& B) const noexcept
	{
		if constexpr (BRows != ROWS || BCols != COLUMNS)
		{
			// Can't put a limitation on '==' comperasion operator when rows or columns are not equal.
			// Because it still got a meaning: not equal.
			return false;
		}
		else
		{
			for (size_t i = 0; i < ROWS; i++)
			{
				for (size_t j = 0; j < COLUMNS; j++)
				{
					if (B[i][j] != _data[i][j])
						return false;
				}
			}

			return true;
		}
	}
	template <size_t BCols> constexpr decltype(auto) operator*(const Matrix<COLUMNS, BCols>& B) const noexcept
	{
		Matrix<ROWS, BCols> res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < BCols; j++)
			{
				res[i][j] = 0;
				for (size_t k = 0; k < COLUMNS; k++)
				{
					res[i][j] += _data[i][k] * B[k][j];
				}
			}
		}

		return res;
	}
	constexpr decltype(auto) operator+(const This_t& B) const noexcept
	{
		This_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = _data[i][j] + B[i][j];
			}
		}

		return res;
	}
	constexpr decltype(auto) operator+=(const This_t& B) noexcept { return (*this = *this + B); }
	constexpr decltype(auto) operator-(const This_t& B) const noexcept
	{
		This_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = _data[i][j] - B[i][j];
			}
		}

		return res;
	}
	constexpr decltype(auto) operator-=(const This_t& B) noexcept { return (*this = *this - B); }
	template <size_t BCols> constexpr decltype(auto) operator|(const Matrix<ROWS, BCols>& B) const noexcept	// Direct combine. Such that I|J|K == M3x3::Identity.
	{
		constexpr size_t C_COLS = COLUMNS + BCols;

		Matrix<ROWS, C_COLS> m;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < C_COLS; j++)
			{
				m[i][j] = j < COLUMNS ? _data[i][j] : B[i][j - COLUMNS];
			}
		}

		return m;
	}
	//
	// Between matrix and scalar.
	constexpr decltype(auto) operator*(mxs_t fl) const noexcept
	{
		This_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = _data[i][j] * fl;
			}
		}

		return res;
	}
	constexpr decltype(auto) operator/(mxs_t fl) const noexcept
	{
		This_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = _data[i][j] / fl;
			}
		}

		return res;
	}
	constexpr decltype(auto) operator*=(mxs_t fl) noexcept { return (*this = *this * fl); }
	constexpr decltype(auto) operator/=(mxs_t fl) noexcept { return (*this = *this / fl); }
	//
	// Between matrix and vector.
	constexpr Vector2D operator*(const Vector2D& v) const noexcept requires(COLUMNS >= 2U)
	{
		Matrix<COLUMNS, 1> matrixlise_v2;
		matrixlise_v2[0][0] = v.x;
		matrixlise_v2[1][0] = v.y;

		if constexpr (COLUMNS > 2U)
		{
			for (size_t i = 2; i < COLUMNS; i++)
			{
				matrixlise_v2[i][0] = 1;	// Fill the rest part with a dummy 1. NOT A ZERO!
			}
		}

		auto result = *this * matrixlise_v2;

		return Vector2D(result[0][0], result[1][0]);
	}
	constexpr Vector operator*(const Vector& v) const noexcept requires(COLUMNS >= 3U)
	{
		Matrix<COLUMNS, 1> matrixlise_v3;
		matrixlise_v3[0][0] = v.x;
		matrixlise_v3[1][0] = v.y;
		matrixlise_v3[2][0] = v.z;

		if constexpr (COLUMNS > 3U)
		{
			for (size_t i = 3; i < COLUMNS; i++)
			{
				matrixlise_v3[i][0] = 1;	// Fill the rest part with a dummy 1. NOT A ZERO!
			}
		}

		auto result = *this * matrixlise_v3;

		return Vector(result[0][0], result[1][0], result[2][0]);
	}
	constexpr Matrix<3, 3> operator|(const Vector2D& v) const noexcept requires(ROWS == 2U && COLUMNS == 2U)
	{
		return Matrix<3, 3>({
			{_data[0][0], _data[0][1], v.x},
			{_data[1][0], _data[1][1], v.y},
			{0, 0, 1}
		});
	}
	constexpr Matrix<4, 4> operator|(const Vector& v) const noexcept requires(ROWS == 3U && COLUMNS == 3U)
	{
		return Matrix<4, 4>({
			{_data[0][0], _data[0][1], _data[0][2], v.x},
			{_data[1][0], _data[1][1], _data[1][2], v.y},
			{_data[2][0], _data[2][1], _data[2][2], v.z},
			{0, 0, 0, 1}
		});
	}
	//
	// Shortcut operator(related to math symbol)
	constexpr decltype(auto) operator~() const noexcept requires(SQUARE_MX) { return Inverse(); }
	constexpr decltype(auto) operator-() const noexcept
	{
		This_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = -_data[i][j];
			}
		}

		return res;
	}
	//
	// Conversion
	constexpr decltype(auto) ToVector(size_t c = 0U) const noexcept
	{
		assert(c < COLUMNS);

		// Vectors are written vertically in matrices.
		if constexpr (ROWS == 2U)
			return Vector2D(_data[0][c], _data[1][c]);
		else if constexpr (ROWS == 3U)
			return Vector(_data[0][c], _data[1][c], _data[2][c]);
		else
		{
			Matrix<ROWS, 1> m;

			for (size_t i = 0U; i < ROWS; i++)
				m[i][0] = _data[i][c];

			return m;
		}
	}

	// STL Containers Compatibility
	// Iterators
	using value_type = mxs_t [COLUMNS];
	using iterator = _STD _Array_iterator<value_type, ROWS>;
	using const_iterator = _STD _Array_const_iterator<value_type, ROWS>;
	using reverse_iterator = _STD reverse_iterator<iterator>;
	using const_reverse_iterator = _STD reverse_iterator<const_iterator>;
	[[nodiscard]] constexpr iterator begin(void) noexcept { return iterator(&_data[0], 0); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator begin(void) const noexcept { return const_iterator(&_data[0], 0); }
	[[nodiscard]] constexpr iterator end(void) noexcept { return iterator(&_data[0], ROWS); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator end(void) const noexcept { return const_iterator(&_data[0], ROWS); }
	[[nodiscard]] constexpr reverse_iterator rbegin(void) noexcept { return reverse_iterator(end()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rbegin(void) const noexcept { return const_reverse_iterator(end()); }
	[[nodiscard]] constexpr reverse_iterator rend(void) noexcept { return reverse_iterator(begin()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rend(void) const noexcept { return const_reverse_iterator(begin()); }
	[[nodiscard]] constexpr const_iterator cbegin(void) const noexcept { return begin(); }
	[[nodiscard]] constexpr const_iterator cend(void) const noexcept { return end(); }
	[[nodiscard]] constexpr const_reverse_iterator crbegin(void) const noexcept { return rbegin(); }
	[[nodiscard]] constexpr const_reverse_iterator crend(void) const noexcept { return rend(); }

	// Element Access
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using reference = value_type&;
	using const_reference = value_type const&;
	using pointer = value_type*;
	using const_pointer = value_type const*;
	[[nodiscard]] constexpr reference at(size_type pos)
	{
		if (pos >= ROWS)
			throw std::out_of_range(std::format("[Matrix<{}, {}>::at] Invalid accessing pos: {}.", ROWS, COLUMNS, pos));

		return _data[pos];
	}
	[[nodiscard]] constexpr const_reference at(size_type pos) const	// #UPDATE_AT_CPP23 explict this
	{
		if (pos >= ROWS)
			throw std::out_of_range(std::format("[Matrix<{}, {}>::at] Invalid accessing pos: {}.", ROWS, COLUMNS, pos));

		return _data[pos];

	}
	[[nodiscard]] constexpr reference operator[] (size_type pos) noexcept { return _data[pos]; }
	[[nodiscard]] constexpr const_reference operator[] (size_type pos) const noexcept { return _data[pos]; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr pointer data(void) noexcept { return &_data[0]; }
	[[nodiscard]] constexpr const_pointer data(void) const noexcept { return &_data[0]; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference front(void) noexcept { return _data[0]; }
	[[nodiscard]] constexpr const_reference front(void) const noexcept { return _data[0]; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference back(void) noexcept { return _data[std::max<size_type>(0, ROWS - 1)]; }
	[[nodiscard]] constexpr const_reference back(void) const noexcept { return _data[std::max<size_type>(0, ROWS - 1)]; }	// #UPDATE_AT_CPP23 explict this

	// Capacity
	[[nodiscard]] static consteval bool empty(void) noexcept { return false; }
	[[nodiscard]] static consteval size_type size(void) noexcept { return RxC; }
	[[nodiscard]] static consteval size_type max_size(void) noexcept { return RxC; }

	// Modifiers
	constexpr void fill(const mxs_t& val) noexcept { for (auto& Row : _data) for (auto& Cell : Row) Cell = 0; }
	constexpr void swap(This_t& other) noexcept { [&] <size_t... I>(std::index_sequence<I...>&&) constexpr { (std::swap(_data[I], other._data[I]), ...); }(std::make_index_sequence<ROWS>{}); }

private:
	mxs_t _data[ROWS][COLUMNS];
};

export template <size_t _rows, size_t _cols> constexpr auto operator*(mxs_t fl, const Matrix<_rows, _cols>& m) noexcept
{
	return m * fl;
}

#ifdef _IOSTREAM_
export template <size_t _rows, size_t _cols> std::ostream& operator<<(std::ostream& o, const Matrix<_rows, _cols>& m) noexcept
{
	for (size_t i = 0; i < _rows; i++)
	{
		for (size_t j = 0; j < _cols; j++)
		{
			o << m[i][j] << std::setw(10);
		}

		o << std::endl << std::left;
	}

	o << '\n' << std::left;
	return o;
}
#endif // _IOSTREAM_

export using qtn_t = double;
export inline constexpr auto QTN_EPSILON = std::numeric_limits<qtn_t>::epsilon();
export inline constexpr auto QTN_NAN = std::numeric_limits<qtn_t>::quiet_NaN();
export inline constexpr auto QTN_INFINITY = std::numeric_limits<qtn_t>::infinity();

export struct Quaternion
{
	// Constructors
	constexpr Quaternion() noexcept : a{ 1 }, b{}, c{}, d{} {}	// Identity.
	constexpr Quaternion(qtn_t W, qtn_t X, qtn_t Y, qtn_t Z) noexcept : a(W), b(X), c(Y), d(Z) {}
	constexpr Quaternion(qtn_t yaw, qtn_t pitch, qtn_t roll) noexcept // yaw (Z), pitch (Y), roll (X)
	{
		yaw *= std::numbers::pi / 180.0;
		pitch *= std::numbers::pi / 180.0;
		roll *= std::numbers::pi / 180.0;

		auto const cy = gcem::cos(yaw * 0.5);
		auto const sy = gcem::sin(yaw * 0.5);
		auto const cp = gcem::cos(pitch * 0.5);
		auto const sp = gcem::sin(pitch * 0.5);
		auto const cr = gcem::cos(roll * 0.5);
		auto const sr = gcem::sin(roll * 0.5);

		a = cr * cp * cy + sr * sp * sy;
		b = sr * cp * cy - cr * sp * sy;
		c = cr * sp * cy + sr * cp * sy;
		d = cr * cp * sy - sr * sp * cy;
	}
	constexpr Quaternion(const Vector& vecAxis, qtn_t degree) noexcept	// Axis must be a unit vector. In clockwise if works in right-handed coordinate system.
	{
		degree *= std::numbers::pi / 180.0;
		auto cosine = gcem::cos(0.5 * degree);
		auto sine = gcem::sin(0.5 * degree);

		a = cosine;
		b = vecAxis.x * sine;
		c = vecAxis.y * sine;
		d = vecAxis.z * sine;
	}
	constexpr Quaternion(const Vector &vecFrom, const Vector &vecTo) noexcept
	{
		auto const vecCross = CrossProduct(vecFrom, vecTo);
		auto const flMagnitude = Hydrogenium::sqrt(a * a + b * b + c * c + d * d);

		*this = Quaternion(
			(Hydrogenium::sqrt(vecFrom.LengthSquared() * vecTo.LengthSquared()) + DotProduct(vecFrom, vecTo)) / flMagnitude,
			vecCross.x / flMagnitude,
			vecCross.y / flMagnitude,
			vecCross.z / flMagnitude
		);
	}
	explicit constexpr Quaternion(const Vector& vecEulerAngles) : Quaternion(vecEulerAngles.yaw, vecEulerAngles.pitch, vecEulerAngles.roll) {}
	explicit constexpr Quaternion(const Matrix<3, 3>& m) noexcept	// 'm' must be a pure rotation matrix! 
	{
		a = gcem::sqrt(1.0 + m[0][0] + m[1][1] + m[2][2]) / 2.0;
		b = (m[2][1] - m[1][2]) / (4 * a);
		c = (m[0][2] - m[2][0]) / (4 * a);
		d = (m[1][0] - m[0][1]) / (4 * a);
	}
	explicit constexpr Quaternion(std::ranges::range auto&& RangeObj) noexcept requires(std::convertible_to<decltype(*std::begin(RangeObj)), qtn_t>) : a{}, b{}, c{}, d{} { for (auto&& [Ref, Val] : ::ranges::views::zip(*this, RangeObj)) Ref = static_cast<qtn_t>(Val); }	// #UPDATE_AT_CPP26 ranges::enumerator

	// Static Methods
	static consteval decltype(auto) Zero() noexcept { return Quaternion(0, 0, 0, 0); }
	static consteval decltype(auto) Identity() noexcept { return Quaternion(1, 0, 0, 0); }
	static consteval decltype(auto) I() noexcept { return Quaternion(0, 1, 0, 0); }
	static consteval decltype(auto) J() noexcept { return Quaternion(0, 0, 1, 0); }
	static consteval decltype(auto) K() noexcept { return Quaternion(0, 0, 0, 1); }

	// Scalar Operations
	constexpr decltype(auto) operator*(qtn_t x) const noexcept { return Quaternion(a * x, b * x, c * x, d * x); }
	constexpr decltype(auto) operator*=(qtn_t x) noexcept { return (*this = *this * x); }
	constexpr decltype(auto) operator/(qtn_t x) const noexcept { return Quaternion(a / x, b / x, c / x, d / x); }
	constexpr decltype(auto) operator/=(qtn_t x) noexcept { return (*this = *this / x); }

	// Properties
	inline constexpr decltype(auto) Norm() const noexcept { return Hydrogenium::sqrt(a * a + b * b + c * c + d * d); }	// a.k.a. magnitude
	inline constexpr decltype(auto) NormSquared() const noexcept { return (a * a + b * b + c * c + d * d); }
	inline constexpr decltype(auto) Conjugate() const noexcept { return Quaternion(a, -b, -c, -d); }
	inline constexpr decltype(auto) Versor() const noexcept { return *this / Norm(); }
	inline constexpr decltype(auto) Reciprocal() const noexcept { return Conjugate() / NormSquared(); }
	inline constexpr decltype(auto) Real() const noexcept { return a; }
	inline constexpr decltype(auto) Pure() const noexcept { return Vector(b, c, d); }

	// Methods
	constexpr bool IsNaN() const noexcept { return Hydrogenium::is_nan(a, b, c, d); }
	constexpr bool IsZero() const noexcept { return Approx(Zero(), QTN_EPSILON); }
	constexpr bool Approx(const Quaternion& q, qtn_t tolerance = QTN_EPSILON) const noexcept
	{
		return gcem::abs(a - q.a) <= tolerance
			&& gcem::abs(b - q.b) <= tolerance
			&& gcem::abs(c - q.c) <= tolerance
			&& gcem::abs(d - q.d) <= tolerance;
	}

	// Operators
	constexpr decltype(auto) operator-() const noexcept { return Quaternion{ -a, -b, -c, -d }; }
	constexpr decltype(auto) operator~() const noexcept { return Reciprocal(); }
	constexpr decltype(auto) operator==(const Quaternion& q) const noexcept { return Approx(q, QTN_EPSILON); }
	constexpr decltype(auto) operator==(qtn_t n) const noexcept { return *this == Quaternion(n, 0, 0, 0); }

	// Transformation
	constexpr decltype(auto) operator*(const Quaternion& q) const noexcept
	{
		return Quaternion{
			a * q.a - b * q.b - c * q.c - d * q.d,
			b * q.a + a * q.b + c * q.d - d * q.c,
			a * q.c - b * q.d + c * q.a + d * q.b,
			a * q.d + b * q.c - c * q.b + d * q.a
		};
	}
	constexpr decltype(auto) operator*=(const Quaternion& q) noexcept { return (*this = *this * q); }
	constexpr decltype(auto) operator*(const Vector& v) const noexcept { return 2.0 * DotProduct(Pure(), v) * Pure() + (a * a - Pure().LengthSquared()) * v + 2.0 * a * CrossProduct(Pure(), v); }	// Rotate a vector by this quaternion.

	// Access
	constexpr qtn_t& operator[](size_t index) noexcept { assert(index < 4); return ((qtn_t*)(&a))[index]; }
	constexpr const qtn_t operator[](size_t index) const noexcept { assert(index < 4); return ((const qtn_t*)(&a))[index]; }

	// Conversion
	constexpr Vector Euler() const noexcept
	{
		Vector vecAngles;

		// roll (x-axis rotation)
		auto sinr_cosp = 2 * (a * b + c * d);
		auto cosr_cosp = 1 - 2 * (b * b + c * c);
		vecAngles.roll = (vec_t)gcem::atan2(sinr_cosp, cosr_cosp);

		// pitch (y-axis rotation)
		auto sinp = 2 * (a * c - d * b);
		if (gcem::abs(sinp) >= 1)
			vecAngles.pitch = (vec_t)gcem::copysign(std::numbers::pi / 2.0, sinp); // use 90 degrees if out of range
		else
			vecAngles.pitch = (vec_t)gcem::asin(sinp);

		// yaw (z-axis rotation)
		auto siny_cosp = 2 * (a * d + b * c);
		auto cosy_cosp = 1 - 2 * (c * c + d * d);
		vecAngles.yaw = (vec_t)gcem::atan2(siny_cosp, cosy_cosp);

		// Rad to Deg
		vecAngles *= 180.0 / std::numbers::pi;

		return vecAngles;
	}
	constexpr Matrix<3, 3> M3x3() const noexcept
	{
		return Matrix<3, 3>({
			{a * a + b * b - c * c - d * d, 2.0 * (b * c - a * d), 2.0 * (b * d + a * c)},
			{2.0 * (b * c + a * d), a * a - b * b + c * c - d * d, 2.0 * (c * d - a * b)},
			{2.0 * (b * d - a * c), 2.0 * (c * d + a * b), a * a - b * b - c * c + d * d}
		});
	}

	// STL Containers Compatibility
	// Iterators
	using value_type = qtn_t;
	using iterator = _STD _Array_iterator<value_type, 4>;
	using const_iterator = _STD _Array_const_iterator<value_type, 4>;
	using reverse_iterator = _STD reverse_iterator<iterator>;
	using const_reverse_iterator = _STD reverse_iterator<const_iterator>;
	[[nodiscard]] constexpr iterator begin(void) noexcept { return iterator(&a, 0); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator begin(void) const noexcept { return const_iterator(&a, 0); }
	[[nodiscard]] constexpr iterator end(void) noexcept { return iterator(&a, 4); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_iterator end(void) const noexcept { return const_iterator(&a, 4); }
	[[nodiscard]] constexpr reverse_iterator rbegin(void) noexcept { return reverse_iterator(end()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rbegin(void) const noexcept { return const_reverse_iterator(end()); }
	[[nodiscard]] constexpr reverse_iterator rend(void) noexcept { return reverse_iterator(begin()); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr const_reverse_iterator rend(void) const noexcept { return const_reverse_iterator(begin()); }
	[[nodiscard]] constexpr const_iterator cbegin(void) const noexcept { return begin(); }
	[[nodiscard]] constexpr const_iterator cend(void) const noexcept { return end(); }
	[[nodiscard]] constexpr const_reverse_iterator crbegin(void) const noexcept { return rbegin(); }
	[[nodiscard]] constexpr const_reverse_iterator crend(void) const noexcept { return rend(); }

	// Element Access
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;
	[[nodiscard]] constexpr reference at(size_type pos)
	{
		switch (pos)
		{
		case 0:
			return a;
		case 1:
			return b;
		case 2:
			return c;
		case 3:
			return d;
		[[unlikely]] default:
			throw std::out_of_range(std::format("[Quaternion::at] Invalid accessing pos: {}.", pos));
		}
	}
	[[nodiscard]] constexpr const_reference at(size_type pos) const	// #UPDATE_AT_CPP23 explict this
	{
		switch (pos)
		{
		case 0:
			return a;
		case 1:
			return b;
		case 2:
			return c;
		case 3:
			return d;
		[[unlikely]] default:
			throw std::out_of_range(std::format("[Quaternion::at] Invalid accessing pos: {}.", pos));
		}
	}
	//[[nodiscard]] constexpr reference operator[] (std::size_t pos) noexcept { return *((&a) + pos); }
	//[[nodiscard]] constexpr const_reference operator[] (std::size_t pos) const noexcept { return *((&a) + pos); }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr pointer data(void) noexcept { return &a; }
	[[nodiscard]] constexpr const_pointer data(void) const noexcept { return &a; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference front(void) noexcept { return a; }
	[[nodiscard]] constexpr const_reference front(void) const noexcept { return a; }	// #UPDATE_AT_CPP23 explict this
	[[nodiscard]] constexpr reference back(void) noexcept { return d; }
	[[nodiscard]] constexpr const_reference back(void) const noexcept { return d; }	// #UPDATE_AT_CPP23 explict this

	// Capacity
	[[nodiscard]] static consteval bool empty(void) noexcept { return false; }
	[[nodiscard]] static consteval size_type size(void) noexcept { return static_cast<size_type>(4); }
	[[nodiscard]] static consteval size_type max_size(void) noexcept { return static_cast<size_type>(4); }

	// Modifiers
	constexpr void fill(const_reference val) noexcept { a = b = c = d = val; }
	constexpr void swap(Quaternion& other) noexcept { _STD _Swap_ranges_unchecked(&a, (&a) + 4, &other.a); }

	// Members
	qtn_t a, b, c, d;	// w, x, y, z
};

export constexpr auto operator*(qtn_t fl, const Quaternion& q) noexcept { return q * fl; }	// Scalar multiplication is commutative, but nothing else.
export constexpr auto operator==(qtn_t n, const Quaternion& q) noexcept { return q == n; }

#ifdef _IOSTREAM_
export std::ostream& operator<<(std::ostream& o, const Quaternion& q) noexcept
{
	o << "W: " << q.a << std::endl;
	o << "X: " << q.b << std::endl;
	o << "Y: " << q.c << std::endl;
	o << "Z: " << q.d << std::endl;
	return o;
}
#endif // _IOSTREAM_
