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

// C++
#include <array>
#include <bit>
#include <cassert>
#include <concepts>
#include <format>
#include <iomanip>
#include <iostream>
#include <limits>
#include <numbers>

// Static math lib
#include "gcem/include/gcem.hpp"

export module UtlLinearAlgebra;


import UtlArithmetic;
import UtlConcepts;

// Concepts for this module.
template<typename A> concept ProperArray2 = requires(A array) { requires array.max_size() >= 2U; };
template<typename A> concept ProperArray3 = requires(A array) { requires array.max_size() >= 3U; };

export using vec_t = float;
constexpr auto VEC_EPSILON = std::numeric_limits<vec_t>::epsilon();
constexpr auto VEC_NAN = std::numeric_limits<vec_t>::quiet_NaN();
constexpr auto VEC_INFINITY = std::numeric_limits<vec_t>::infinity();

// Used for many pathfinding and many other operations that are treated as planar rather than 3D.
export struct Vector2D
{
	// Construction
	constexpr Vector2D() noexcept : x(0), y(0) {}
	constexpr Vector2D(Arithmetic auto X, Arithmetic auto Y) noexcept : x(static_cast<vec_t>(X)), y(static_cast<vec_t>(Y)) {}
	explicit constexpr Vector2D(Arithmetic auto sideLength) noexcept : width(static_cast<vec_t>(sideLength)), height(static_cast<vec_t>(sideLength)) {}
	template<Arithmetic T, std::size_t size> requires(size >= 2U) explicit constexpr Vector2D(const T (&rgfl)[size]) noexcept : x(static_cast<vec_t>(rgfl[0])), y(static_cast<vec_t>(rgfl[1])) {}
	constexpr Vector2D(std::initializer_list<vec_t>&& lst) noexcept { assert(lst.size() >= 2U); auto it = lst.begin(); x = *it++; y = *it++; }

	// Operators
	constexpr decltype(auto) operator-() const noexcept { return Vector2D(-x, -y); }
	constexpr bool operator==(const Vector2D& v) const noexcept { return gcem::abs(x - v.x) < VEC_EPSILON && gcem::abs(y - v.y) < VEC_EPSILON; }
	constexpr std::strong_ordering operator<=> (const Vector2D& v) const noexcept { auto const lhs = Length(), rhs = v.Length(); return lhs < rhs ? std::strong_ordering::less : lhs > rhs ? std::strong_ordering::greater : std::strong_ordering::equal; }
	constexpr std::strong_ordering operator<=> (Arithmetic auto fl) const noexcept { auto const l = static_cast<decltype(fl)>(Length()); return l < fl ? std::strong_ordering::less : l > fl ? std::strong_ordering::greater : std::strong_ordering::equal; }

	constexpr decltype(auto) operator=(std::nullptr_t) noexcept { return Zero(); }

	constexpr decltype(auto) operator+(const Vector2D& v) const noexcept { return Vector2D(x + v.x, y + v.y); }
	constexpr decltype(auto) operator-(const Vector2D& v) const noexcept { return Vector2D(x - v.x, y - v.y); }
	constexpr decltype(auto) operator+=(const Vector2D& v) noexcept { return (*this = *this + v); }
	constexpr decltype(auto) operator-=(const Vector2D& v) noexcept { return (*this = *this - v); }

	constexpr decltype(auto) operator*(Arithmetic auto fl) const noexcept { return Vector2D(x * fl, y * fl); }
	constexpr decltype(auto) operator/(Arithmetic auto fl) const noexcept { return Vector2D(x / fl, y / fl); }
	constexpr decltype(auto) operator*=(Arithmetic auto fl) noexcept { return (*this = *this * fl); }
	constexpr decltype(auto) operator/=(Arithmetic auto fl) noexcept { return (*this = *this / fl); }

	// Static methods
	static constexpr Vector2D Zero() noexcept { return Vector2D(0, 0); }
	static constexpr Vector2D I() noexcept { return Vector2D(1, 0); }
	static constexpr Vector2D J() noexcept { return Vector2D(0, 1); }

	// Methods
	inline void Clear() { x = 0; y = 0; }
	inline void CopyToIter(ProperIter auto it) const noexcept { *it++ = x; *it++ = y; }
	inline void CopyToArray(ProperArray2 auto arr) const noexcept { arr[0] = x; arr[1] = y; }
	constexpr float Length() const noexcept { return 1.0f / Hydrogenium::rsqrt(x * x + y * y); }	// Get the vector's magnitude
	constexpr vec_t LengthSquared() const noexcept { return x * x + y * y; }	// Get the vector's magnitude squared
	constexpr Vector2D Normalize() const noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
			return Zero();

		auto invsqrt = Hydrogenium::rsqrt(x * x + y * y);
		return Vector2D(x * invsqrt, y * invsqrt);
	}
	constexpr float NormalizeInPlace() noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
		{
			x = 1;
			y = 0;

			return 0.0f;
		}

		auto fl = Hydrogenium::rsqrt(x * x + y * y);

		x *= fl;
		y *= fl;

		return 1.0f / fl;
	}
	constexpr Vector2D SetLength(Arithmetic auto newlen) const noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
			return Zero();

		auto fl = static_cast<float>(newlen) * Hydrogenium::rsqrt(x * x + y * y);
		return Vector2D(x * fl, y * fl);
	}
	constexpr void SetLengthInPlace(Arithmetic auto newlen) noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
		{
			x = 0;
			y = 0;

			return;
		}

		auto fl = static_cast<float>(newlen) * Hydrogenium::rsqrt(x * x + y * y);

		x *= fl;
		y *= fl;
	}
	constexpr bool IsZero(vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return (
			x > -tolerance && x < tolerance &&
			y > -tolerance && y < tolerance
		);
	}
	constexpr bool IsNaN() const noexcept { return Hydrogenium::is_nan(x, y); }

	// Conversion
	constexpr operator float* () noexcept { return &x; } // Vectors will now automatically convert to float * when needed
	constexpr operator const float* () const noexcept { return &x; } // Vectors will now automatically convert to float * when needed

	explicit constexpr operator bool () const noexcept { return !IsZero(); }	// Can be placed in if() now.
	explicit constexpr operator float() const noexcept { return Length(); }

	// Linear Algebra
	// Rotate in counter-clockwise. Angles in degree.
	constexpr Vector2D Rotate(Arithmetic auto angle) const noexcept
	{
		auto a = (static_cast<double>(angle) * std::numbers::pi / 180.0);
		auto c = gcem::cos(a);
		auto s = gcem::sin(a);

		return Vector2D(
			c * x - s * y,
			s * x + c * y
		);
	}

	// Members
	union { vec_t x; vec_t width; };
	union { vec_t y; vec_t height; };
};

export constexpr auto DotProduct(const Vector2D& a, const Vector2D& b) noexcept
{
	return (a.x * b.x + a.y * b.y);
}

export constexpr Vector2D operator*(Arithmetic auto fl, const Vector2D& v) noexcept
{
	return v * fl;
}

export constexpr decltype(auto) operator^(const Vector2D& lhs, const Vector2D& rhs) noexcept	// Get the angle between two vectors. Returns an angle in degree.
{
	double length_ab = lhs.Length() * rhs.Length();

	if (length_ab < DBL_EPSILON)
		return 0.0;

	return gcem::acos(DotProduct(lhs, rhs) / length_ab) * (180.0 / std::numbers::pi);
}

#ifdef _IOSTREAM_
export std::ostream& operator<<(std::ostream& o, const Vector2D& v) noexcept
{
	o << "X: " << v.x << std::endl;
	o << "Y: " << v.y << std::endl;
	return o;
}
#endif // _IOSTREAM_

// 3D Vector
// Same data-layout as engine's vec3_t, which is a vec_t[3]
export struct Vector
{
	// Construction
	constexpr Vector() noexcept : x(0), y(0), z(0) {}
	constexpr Vector(Arithmetic auto X, Arithmetic auto Y, Arithmetic auto Z) noexcept : x(static_cast<vec_t>(X)), y(static_cast<vec_t>(Y)), z(static_cast<vec_t>(Z)) {}
	constexpr Vector(const Vector2D& v2d, Arithmetic auto Z) noexcept : x(v2d.x), y(v2d.y), z(static_cast<vec_t>(Z)) {}
	template<Arithmetic T, std::size_t size> requires(size >= 3U) explicit constexpr Vector(const T(&rgfl)[size]) noexcept : x(static_cast<vec_t>(rgfl[0])), y(static_cast<vec_t>(rgfl[1])), z(static_cast<vec_t>(rgfl[2])) {}
	constexpr Vector(std::initializer_list<vec_t>&& lst) noexcept { assert(lst.size() >= 3U); auto it = lst.begin(); x = *it++; y = *it++; z = *it++; }

	// Operators
	constexpr decltype(auto) operator-() const noexcept { return Vector(-x, -y, -z); }
	constexpr bool operator==(const Vector& v) const noexcept { return gcem::abs(x - v.x) < VEC_EPSILON && gcem::abs(y - v.y) < VEC_EPSILON && gcem::abs(z - v.z) < VEC_EPSILON; }
	constexpr std::strong_ordering operator<=> (const Vector& v) const noexcept { auto const lhs = Length(), rhs = v.Length(); return lhs < rhs ? std::strong_ordering::less : lhs > rhs ? std::strong_ordering::greater : std::strong_ordering::equal; }
	constexpr std::strong_ordering operator<=> (Arithmetic auto fl) const noexcept { auto const l = static_cast<decltype(fl)>(Length()); return l < fl ? std::strong_ordering::less : l > fl ? std::strong_ordering::greater : std::strong_ordering::equal; }

	constexpr decltype(auto) operator=(std::nullptr_t) noexcept { return Zero(); }

	constexpr decltype(auto) operator+(const Vector& v) const noexcept { return Vector(x + v.x, y + v.y, z + v.z); }
	constexpr decltype(auto) operator-(const Vector& v) const noexcept { return Vector(x - v.x, y - v.y, z - v.z); }
	constexpr decltype(auto) operator+=(const Vector& v) noexcept { return (*this = *this + v); }
	constexpr decltype(auto) operator-=(const Vector& v) noexcept { return (*this = *this - v); }

	constexpr decltype(auto) operator*(Arithmetic auto fl) const noexcept { return Vector(x * fl, y * fl, z * fl); }
	constexpr decltype(auto) operator/(Arithmetic auto fl) const noexcept { return Vector(x / fl, y / fl, z / fl); }
	constexpr decltype(auto) operator*=(Arithmetic auto fl) noexcept { return (*this = *this * fl); }
	constexpr decltype(auto) operator/=(Arithmetic auto fl) noexcept { return (*this = *this / fl); }

	// Static methods
	static constexpr Vector Zero() noexcept { return Vector(0, 0, 0); }
	static constexpr Vector I() noexcept { return Vector(1, 0, 0); }
	static constexpr Vector J() noexcept { return Vector(0, 1, 0); }
	static constexpr Vector K() noexcept { return Vector(0, 0, 1); }

	// Methods
	inline void Clear() { x = y = z = 0; }
	inline void CopyToIter(ProperIter auto it) const noexcept { *it++ = x; *it++ = y; *it++ = z; }
	inline void CopyToArray(ProperArray3 auto arr) const noexcept { arr[0] = x; arr[1] = y; arr[2] = z; }
	constexpr float Length() const noexcept { return 1.0f / Hydrogenium::rsqrt(x * x + y * y + z * z); }	// Get the vector's magnitude
	constexpr vec_t LengthSquared() const noexcept { return (x * x + y * y + z * z); }	// Get the vector's magnitude squared
	constexpr float Length2D() const noexcept { return 1.0f / Hydrogenium::rsqrt(x * x + y * y); }	// Get the vector's magnitude, but only consider its X and Y component
	constexpr float Length2DSquared() const noexcept { return (x * x + y * y); }
	constexpr Vector Normalize() const noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
			return Zero();

		auto invsqrt = Hydrogenium::rsqrt(x * x + y * y + z * z);
		return Vector(x * invsqrt, y * invsqrt, z * invsqrt);
	}
	constexpr float NormalizeInPlace() noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
		{
			x = 1;
			y = 0;
			z = 0;

			return 0.0f;
		}

		auto fl = Hydrogenium::rsqrt(x * x + y * y + z * z);

		x *= fl;
		y *= fl;
		z *= fl;

		return 1.0f / fl;
	}
	constexpr Vector SetLength(Arithmetic auto newlen) const noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
			return Zero();

		auto fl = static_cast<float>(newlen) * Hydrogenium::rsqrt(x * x + y * y + z * z);
		return Vector(x * fl, y * fl, z * fl);
	}
	constexpr void SetLengthInPlace(Arithmetic auto newlen) noexcept
	{
		if (LengthSquared() <= VEC_EPSILON)
		{
			x = 0;
			y = 0;
			z = 0;

			return;
		}

		auto fl = static_cast<float>(newlen) * Hydrogenium::rsqrt(x * x + y * y + z * z);

		x *= fl;
		y *= fl;
		z *= fl;
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

	// Conversion
	constexpr operator float* () noexcept { return &x; } // Vectors will now automatically convert to float * when needed
	constexpr operator const float* () const noexcept { return &x; } // Vectors will now automatically convert to float * when needed
	
	explicit constexpr operator bool() const noexcept { return !IsZero(); }	// Can be placed in if() now.
	explicit constexpr operator float() const noexcept { return Length(); }

	// Linear Algebra
	// Convert Eular angles to its 'forward' vector.
	constexpr Vector MakeVector() const noexcept
	{
		auto rad_pitch = (pitch * std::numbers::pi / 180.0f);
		auto rad_yaw = (yaw * std::numbers::pi / 180.0f);
		auto tmp = gcem::cos(rad_pitch);

		return Vector(
			-tmp * -gcem::cos(rad_yaw),	// x
			gcem::sin(rad_yaw) * tmp,	// y
			-gcem::sin(rad_pitch)		// z
		);
	}

	// Convert an forward vector to a set of Eular angles.
	constexpr Vector VectorAngles(void) const noexcept
	{
		Vector a;
		a.pitch = 0;
		a.yaw = 0;
		a.roll = 0;

		if (y == 0 && x == 0)
		{
			a.yaw = 0;
			if (z > 0)
				a.pitch = 90;
			else
				a.pitch = 270;
		}
		else
		{
			a.yaw = vec_t(gcem::atan2(-y, x) * 180.0 / std::numbers::pi);
			if (a.yaw < 0)
				a.yaw += 360;

			a.yaw = 360.0f - a.yaw;	// LUNA: why???

			auto tmp = Hydrogenium::rsqrt(x * x + y * y);
			a.pitch = vec_t(gcem::atan(z * tmp) * 180.0 / std::numbers::pi);
			if (a.pitch < 0)
				a.pitch += 360;
		}

		return a;
	}

	constexpr Vector RotateX(float angle) const noexcept
	{
		auto a = (angle * std::numbers::pi / 180.0);
		auto c = gcem::cos(a);
		auto s = gcem::sin(a);

		return Vector(
			x,
			c * y - s * z,
			s * y + c * z
		);
	}

	constexpr Vector RotateY(float angle) const noexcept
	{
		auto a = (angle * std::numbers::pi / 180.0);
		auto c = gcem::cos(a);
		auto s = gcem::sin(a);

		return Vector(
			c * x + s * z,
			y,
			-s * x + c * z
		);
	}

	constexpr Vector RotateZ(float angle) const noexcept
	{
		auto a = (angle * std::numbers::pi / 180.0);
		auto c = gcem::cos(a);
		auto s = gcem::sin(a);

		return Vector(
			c * x - s * y,
			s * x + c * y,
			z
		);
	}

	// Members
	union { vec_t x; vec_t pitch;	};
	union { vec_t y; vec_t yaw;		};
	union { vec_t z; vec_t roll;	};
};

export constexpr Vector operator*(Arithmetic auto fl, const Vector& v) noexcept
{
	return v * fl;
}

export constexpr auto DotProduct(const Vector& a, const Vector& b) noexcept
{
	return (a.x * b.x + a.y * b.y + a.z * b.z);
}

export constexpr auto DotProduct2D(const Vector& a, const Vector& b) noexcept
{
	return (a.x * b.x + a.y * b.y);
}

export constexpr auto CrossProduct(const Vector& a, const Vector& b) noexcept
{
	return Vector(
		a.y * b.z - a.z * b.y,
		a.z * b.x - a.x * b.z,
		a.x * b.y - a.y * b.x
	);
}

// Get the angle between two vectors. Returns an angle in degree.
export constexpr auto operator^(const Vector& a, const Vector& b) noexcept
{
	double length_ab = static_cast<double>(a.Length() * b.Length());

	if (gcem::abs(length_ab) < std::numeric_limits<double>::epsilon())
		return 0.0;

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
constexpr auto MXS_EPSILON = std::numeric_limits<mxs_t>::epsilon();
constexpr auto MXS_NAN = std::numeric_limits<mxs_t>::quiet_NaN();
constexpr auto MXS_INFINITY = std::numeric_limits<mxs_t>::infinity();

export template<size_t _rows = 1U, size_t _cols = 1U>
requires(_rows > 0U && _cols > 0U)
struct Matrix
{
	// Constants
	static constexpr auto ROWS = _rows;
	static constexpr auto COLUMNS = _cols;
	static constexpr bool SQUARE_MX = _rows == _cols;

	// Types
	using row_init_t = std::initializer_list<mxs_t>;
	using this_t = Matrix<ROWS, COLUMNS>;

	// Constructors
	constexpr Matrix() noexcept : _data() {}
	template<Arithmetic T> constexpr Matrix(const T(&array)[ROWS][COLUMNS]) noexcept	// Why can't I use the keyword 'auto' as auto-template here?
	{
		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				_data[i][j] = array[i][j];
			}
		}
	}
	constexpr Matrix(const std::initializer_list<row_init_t>&& list) noexcept
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
	constexpr Matrix(const row_init_t&& list) noexcept
	{
		assert(list.size() >= ROWS * COLUMNS);

		auto iter = list.begin();
		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				assert(iter != list.end());	// list is too short!

				_data[i][j] = *iter;
				iter++;
			}
		}
	}
	template<size_t BRows, size_t BCols> explicit constexpr Matrix(const Matrix<BRows, BCols>& B) noexcept : _data()	// Enforce conversion.
	{
		if constexpr (SQUARE_MX)
		{
			*this = Identity();
		}

		for (size_t i = 0; i < std::min(BRows, ROWS); i++)
		{
			for (size_t j = 0; j < std::min(BCols, COLUMNS); j++)
				_data[i][j] = B[i][j];
		}
	}
	explicit constexpr Matrix(const Vector2D& v) noexcept requires(ROWS >= 2U && COLUMNS == 1U) : _data()
	{
		_data[0][0] = v.x;
		_data[1][0] = v.y;

		if constexpr (ROWS > 2U)
		{
			_data[ROWS - 1U][0] = 1;	// For example, if you wish Vector2(x, y) transcript to matrix<4, 1>, it must be [x, y, 0, 1].
		}
	}
	explicit constexpr Matrix(const Vector& v) noexcept requires(ROWS >= 3U && COLUMNS == 1U) : _data()
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
	static constexpr decltype(auto) Identity() noexcept requires(SQUARE_MX)
	{
		this_t m;

		for (size_t i = 0; i < ROWS; i++)
			m[i][i] = 1;

		return m;
	}
	static constexpr decltype(auto) Zero() noexcept { static const this_t m; return m; }
	static constexpr decltype(auto) Rotation(Arithmetic auto degree) noexcept	// 2D rotation. Ideally generates a 2x2 matrix.
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
			return static_cast<this_t>(	// Use our special defined matrix convert function.
				Matrix<2, 2>({
					{c, -s},
					{s, c}
				})
			);
		}
	}
	static constexpr decltype(auto) Rotation(Arithmetic auto yaw, Arithmetic auto pitch, Arithmetic auto roll) noexcept // 3D rotation. yaw (Z), pitch (Y), roll (X)
	{
		const auto y = yaw / 180.0 * std::numbers::pi, p = pitch / 180.0 * std::numbers::pi, r = roll / 180.0 * std::numbers::pi;
		const auto cy = gcem::cos(y), sy = gcem::sin(y);
		const auto cp = gcem::cos(p), sp = gcem::sin(p);
		const auto cr = gcem::cos(r), sr = gcem::sin(r);

		if constexpr (ROWS == 3U && COLUMNS == 3U)
		{
			return this_t({
				{cy * cp, cy * sp * sr - sy * cr, cy * sp * cr + sy * sr},
				{sy * cp, sy * sp * sr + cy * cr, sy * sp * cr - cy * sr},
				{-sp, cp * sr, cp * cr}
			});
		}
		else
		{
			return static_cast<this_t>(Matrix<3, 3>({
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
			return this_t({
				{c + x * x * (1 - c), x * y * (1 - c) - z * s, x * z * (1 - c) + y * s},
				{y * x * (1 - c) + z * s, c + y * y * (1 - c), y * z * (1 - c) - x * s},
				{z * x * (1 - c) - y * s, z * y * (1 - c) + x * s, c + z * z * (1 - c)}
			});
		}
		else
		{
			return static_cast<this_t>(Matrix<3, 3>({
				{c + x * x * (1 - c), x * y * (1 - c) - z * s, x * z * (1 - c) + y * s},
				{y * x * (1 - c) + z * s, c + y * y * (1 - c), y * z * (1 - c) - x * s},
				{z * x * (1 - c) - y * s, z * y * (1 - c) + x * s, c + z * z * (1 - c)}
			}));
		}
	}
	static constexpr decltype(auto) Scale(mxs_t scale) noexcept requires(SQUARE_MX)
	{
		this_t m;

		for (size_t i = 0; i < ROWS; i++)
			m[i][i] = scale;

		return m;
	}
	static constexpr decltype(auto) Scale(mxs_t x = 1, mxs_t y = 1) noexcept requires(ROWS == 2U && COLUMNS == 2U)
	{
		return this_t({
			{x, 0},
			{0, y}
		});
	}
	static constexpr decltype(auto) Scale(mxs_t x = 1, mxs_t y = 1, mxs_t z = 1) noexcept requires(ROWS == 3U && COLUMNS == 3U)
	{
		return this_t({
			{x, 0, 0},
			{0, y, 0},
			{0, 0, z}
		});
	}
	static constexpr decltype(auto) Translate(mxs_t x = 0, mxs_t y = 0) noexcept requires(ROWS == 3U && COLUMNS == 3U)	// 2D. Translate requires an extra dimension on matrix.
	{
		return this_t({
			{1, 0, x},
			{0, 1, y},
			{0, 0, 1}
		});
	}
	static constexpr decltype(auto) Translate(mxs_t x = 0, mxs_t y = 0, mxs_t z = 0) noexcept requires(ROWS == 4U && COLUMNS == 4U)	// 3D. Translate requires an extra dimension on matrix.
	{
		return this_t({
			{1, 0, 0, x},
			{0, 1, 0, y},
			{0, 0, 1, z},
			{0, 0, 0, 1}
		});
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
		this_t m;

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
			this_t m;

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
	constexpr decltype(auto) ReplaceCol(size_t c, const std::initializer_list<mxs_t>&& list) noexcept
	{
		assert(c < COLUMNS);

		size_t r = 0U;

		for (auto cell : list)
		{
			if (Hydrogenium::is_nan(cell))	// Special key: if a cell is NaN, skip it.
				continue;

			_data[r][c] = cell;
			r++;

			if (r >= ROWS)
				break;
		}
	}
	constexpr decltype(auto) ReplaceRow(size_t r, const std::initializer_list<mxs_t>&& list) noexcept
	{
		assert(r < COLUMNS);

		size_t c = 0U;

		for (auto cell : list)
		{
			if (Hydrogenium::is_nan(cell))	// Special key: if a cell is NaN, skip it.
				continue;

			_data[r][c] = cell;
			c++;

			if (c >= ROWS)
				break;
		}
	}
	constexpr bool IsZero() const noexcept { return *this == this_t::Zero(); }
	constexpr bool IsNaN() const noexcept
	{
		for (const auto& row : _data)
		{
			for (const auto& cell : row)
			{
				if (Hydrogenium::is_nan(cell))
					return true;
			}
		}

		return false;
	}

	// Operators
	// 
	// Between matrices.
	template<size_t BRows, size_t BCols>
	constexpr decltype(auto) operator==(const Matrix<BRows, BCols>& B) const noexcept
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
	template<size_t BRows, size_t BCols>
	constexpr decltype(auto) operator*(const Matrix<BRows, BCols>& B) const noexcept requires(COLUMNS == BRows)
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
	constexpr decltype(auto) operator+(const this_t& B) const noexcept
	{
		this_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = _data[i][j] * B[i][j];
			}
		}

		return res;
	}
	template<size_t BRows, size_t BCols>
	constexpr decltype(auto) operator|(const Matrix<BRows, BCols>& B) const noexcept requires(ROWS == BRows)	// Direct combine. Such that I|J|K == M3x3::Identity.
	{
		constexpr size_t C_COLS = COLUMNS + BCols;

		Matrix<ROWS, C_COLS> m;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < C_COLS; j++)
			{
				m[i][j] = (j < COLUMNS ? _data[i][j] : B[i][j - COLUMNS]);
			}
		}

		return m;
	}
	//
	// Between matrix and scalar.
	constexpr decltype(auto) operator*(Arithmetic auto fl) const noexcept
	{
		this_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = _data[i][j] * fl;
			}
		}

		return res;
	}
	constexpr decltype(auto) operator/(Arithmetic auto fl) const noexcept
	{
		this_t res;

		for (size_t i = 0; i < ROWS; i++)
		{
			for (size_t j = 0; j < COLUMNS; j++)
			{
				res[i][j] = _data[i][j] / fl;
			}
		}

		return res;
	}
	constexpr decltype(auto) operator*=(Arithmetic auto fl) noexcept { return (*this = *this * fl); }
	constexpr decltype(auto) operator/=(Arithmetic auto fl) noexcept { return (*this = *this / fl); }
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
	constexpr Matrix<3, 3> operator|(const Vector2D& v) const noexcept requires(SQUARE_MX && ROWS == 2U)
	{
		return Matrix<3, 3>({
			{_data[0][0], _data[0][1], v.x},
			{_data[1][0], _data[1][1], v.y},
			{0, 0, 1}
		});
	}
	constexpr Matrix<4, 4> operator|(const Vector& v) const noexcept requires(SQUARE_MX && ROWS == 3U)
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
	//
	// Accessor to each cell.
	constexpr mxs_t* operator[](size_t rows) noexcept { assert(rows < ROWS); return &_data[rows][0]; }
	constexpr const mxs_t* operator[](size_t rows) const noexcept { assert(rows < ROWS); return &_data[rows][0]; }

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

private:
	mxs_t _data[ROWS][COLUMNS];
};

export template<size_t _rows, size_t _cols> constexpr auto operator*(Arithmetic auto fl, const Matrix<_rows, _cols>& m) noexcept
{
	return m * fl;
}

#ifdef _IOSTREAM_
export template<size_t _rows, size_t _cols> std::ostream& operator<<(std::ostream& o, const Matrix<_rows, _cols>& m) noexcept
{
	for (size_t i = 0; i < _rows; i++)
	{
		for (size_t j = 0; j < _cols; j++)
		{
			o << m[i][j] << std::setw(10);
		}

		o << std::endl << std::left;
	}

	return o;
}
#endif // _IOSTREAM_

export using qtn_t = double;
constexpr auto QTN_EPSILON = std::numeric_limits<qtn_t>::epsilon();
constexpr auto QTN_NAN = std::numeric_limits<qtn_t>::quiet_NaN();
constexpr auto QTN_INFINITY = std::numeric_limits<qtn_t>::infinity();

export struct Quaternion
{
	constexpr Quaternion() noexcept : a(1), b(0), c(0), d(0) {}	// Identity.
	constexpr Quaternion(Arithmetic auto W, Arithmetic auto X, Arithmetic auto Y, Arithmetic auto Z) noexcept : a(static_cast<qtn_t>(W)), b(static_cast<qtn_t>(X)), c(static_cast<qtn_t>(Y)), d(static_cast<qtn_t>(Z)) {}
	constexpr Quaternion(qtn_t yaw, qtn_t pitch, qtn_t roll) noexcept // yaw (Z), pitch (Y), roll (X)
	{
		yaw *= std::numbers::pi / 180.0;
		pitch *= std::numbers::pi / 180.0;
		roll *= std::numbers::pi / 180.0;

		auto cy = gcem::cos(yaw * 0.5);
		auto sy = gcem::sin(yaw * 0.5);
		auto cp = gcem::cos(pitch * 0.5);
		auto sp = gcem::sin(pitch * 0.5);
		auto cr = gcem::cos(roll * 0.5);
		auto sr = gcem::sin(roll * 0.5);

		a = cr * cp * cy + sr * sp * sy;
		b = sr * cp * cy - cr * sp * sy;
		c = cr * sp * cy + sr * cp * sy;
		d = cr * cp * sy - sr * sp * cy;
	}
	explicit constexpr Quaternion(const Vector& vecEulerAngles) : Quaternion(vecEulerAngles.yaw, vecEulerAngles.pitch, vecEulerAngles.roll) {}
	constexpr Quaternion(const Vector& vecAxis, qtn_t degree) noexcept	// Axis must be a unit vector. In counter-clockwise.
	{
		degree *= std::numbers::pi / 180.0;
		auto cosine = gcem::cos(0.5 * degree);
		auto sine = gcem::sin(0.5 * degree);

		a = cosine;
		b = vecAxis.x * sine;
		c = vecAxis.y * sine;
		d = vecAxis.z * sine;
	}
	explicit constexpr Quaternion(const Matrix<3, 3>& m) noexcept	// 'm' must be a pure rotation matrix! 
	{
		a = gcem::sqrt(1.0 + m[0][0] + m[1][1] + m[2][2]) / 2.0;
		b = (m[2][1] - m[1][2]) / (4 * a);
		c = (m[0][2] - m[2][0]) / (4 * a);
		d = (m[1][0] - m[0][1]) / (4 * a);
	}
	constexpr Quaternion(std::initializer_list<qtn_t>&& lst) noexcept { assert(lst.size() >= 4U); auto it = lst.begin(); a = *it++; b = *it++; c = *it++; d = *it++; }

	// Static Methods
	static constexpr decltype(auto) Zero() noexcept { return Quaternion(0, 0, 0, 0); }
	static constexpr decltype(auto) Identity() noexcept { return Quaternion(1, 0, 0, 0); }

	// Properties
	inline constexpr decltype(auto) Norm() const noexcept { return gcem::sqrt(a * a + b * b + c * c + d * d); }
	inline constexpr decltype(auto) Conjugate() const noexcept { return Quaternion(a, -b, -c, -d); }
	inline constexpr decltype(auto) Versor() const noexcept { return *this / Norm(); }
	inline constexpr decltype(auto) Reciprocal() const noexcept { return Conjugate() / (a * a + b * b + c * c + d * d); }
	inline constexpr decltype(auto) Real() const noexcept { return a; }
	inline constexpr decltype(auto) Pure() const noexcept { return Vector(b, c, d); }

	// Methods
	constexpr bool IsNaN() const noexcept { return Hydrogenium::is_nan(a, b, c, d); }

	// Operators
	constexpr decltype(auto) operator*(const Quaternion& q) const noexcept { return Quaternion(a * q.a - b * q.b - c * q.c - d * q.d, a * q.a + b * q.b + c * q.c - d * q.d, a * q.a - b * q.b + c * q.c + d * q.d, a * q.a + b * q.b - c * q.c + d * q.d); }
	constexpr decltype(auto) operator*=(const Quaternion& q) noexcept { return (*this = *this * q); }

	constexpr decltype(auto) operator*(Arithmetic auto x) const noexcept { return Quaternion(a * x, b * x, c * x, d * x); }
	constexpr decltype(auto) operator*=(Arithmetic auto x) noexcept { return (*this = *this * x); }
	constexpr decltype(auto) operator/(Arithmetic auto x) const noexcept { return Quaternion(a / x, b / x, c / x, d / x); }
	constexpr decltype(auto) operator/=(Arithmetic auto x) noexcept { return (*this = *this / x); }

	constexpr decltype(auto) operator*(const Vector& v) const noexcept { return v + ((CrossProduct(Pure(), v) * a) + CrossProduct(Pure(), CrossProduct(Pure(), v))) * 2.0f; }	// Rotate a vector by this quaternion.

	constexpr qtn_t& operator[](std::integral auto index) noexcept { assert(index < 4); return ((qtn_t*)(&a))[index]; }
	constexpr const qtn_t operator[](std::integral auto index) const noexcept { assert(index < 4); return ((const qtn_t*)(&a))[index]; }

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

	// Members
	qtn_t a, b, c, d;	// w, x, y, z
};

export constexpr auto operator*(Arithmetic auto fl, const Quaternion& q) noexcept { return q * fl; }	// Scalar multiplication is commutative, but nothing else.

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
