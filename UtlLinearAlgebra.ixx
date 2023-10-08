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

export module UtlLinearAlgebra;

// C++
export import <algorithm>;
export import <array>;
export import <concepts>;
export import <format>;
export import <iomanip>;
export import <iostream>;
export import <limits>;
export import <numbers>;
export import <ranges>;
export import <string>;

// Static math lib
export import "gcem/include/gcem.hpp";

// Friendly modules.
export import UtlArithmetic;
export import UtlConcepts;

// Contains preview
template <size_t D> struct Vector;
struct Angles;

export using vec_t = float;
export using real_t = std::conditional_t<sizeof(vec_t) <= 4, double, vec_t>;
export inline constexpr auto VEC_EPSILON = std::numeric_limits<vec_t>::epsilon();
export inline constexpr auto VEC_NAN = std::numeric_limits<vec_t>::quiet_NaN();
export inline constexpr auto VEC_INFINITY = std::numeric_limits<vec_t>::infinity();

export template <size_t iDimension>
struct Vector : std::array<vec_t, iDimension>
{
	using super = std::array<vec_t, iDimension>;

	static inline constexpr auto DIMENSION = iDimension;

	static_assert(DIMENSION > 0, "Buddy, don't have a vector with 0 dimension!");

	// Construction
	constexpr Vector() noexcept = default;
	constexpr Vector(Arithmetic auto... args) noexcept requires(sizeof...(args) <= DIMENSION) : super{ static_cast<vec_t>(args)... } {}
	template <size_t dim_other> constexpr Vector(Vector<dim_other> const& v, Arithmetic auto... args) noexcept requires(dim_other + sizeof...(args) <= DIMENSION) : super() { for (auto&& [Ref, Val] : std::views::zip(*this, v)) Ref = static_cast<vec_t>(Val); [&] <size_t... I>(std::index_sequence<I...>&&) noexcept { (((*this)[dim_other + I] = static_cast<vec_t>(args)), ...); }(std::make_index_sequence<sizeof...(args)>{}); } // #UPDATE_AT_CPP26 std::views::concat
	explicit constexpr Vector(std::ranges::input_range auto&& rgObj) noexcept : super() { for (auto&& [Ref, Val] : std::views::zip(*this, rgObj)) Ref = static_cast<vec_t>(Val); }

	//
	template <size_t COMPONENTS = DIMENSION> requires(COMPONENTS <= iDimension) [[nodiscard]] inline constexpr real_t Length() const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Hydrogenium::sqrt((... + ((*this)[I] * (*this)[I]))); }(std::make_index_sequence<COMPONENTS>{}); }	// Get the vector's magnitude
	template <size_t COMPONENTS = DIMENSION> requires(COMPONENTS <= iDimension) [[nodiscard]] inline constexpr real_t LengthSquared() const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return (... + ((*this)[I] * (*this)[I])); }(std::make_index_sequence<COMPONENTS>{}); }	// Get the vector's magnitude squared
	[[nodiscard]] inline constexpr Vector Normalize() const noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return Zero();

		const auto len = Length();
		return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Vector(((*this)[I] / len)...); }(std::make_index_sequence<DIMENSION>{});
	}
	[[nodiscard]] inline constexpr Vector SetLength(const real_t flNewLength) const noexcept
	{
		if (LengthSquared() <= std::numeric_limits<real_t>::epsilon())
			return Zero();

		const auto fl = flNewLength / Length();
		return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Vector(((*this)[I] * fl)...); }(std::make_index_sequence<DIMENSION>{});
	}
	[[nodiscard]] inline constexpr bool IsZero(const vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept
		{
			return (
				... && ((*this)[I] > -tolerance && (*this)[I] < tolerance)
				);
		}
		(std::make_index_sequence<DIMENSION>{});
	}
	[[nodiscard]] inline constexpr bool IsNaN() const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Hydrogenium::is_nan((*this)[I]...); }(std::make_index_sequence<DIMENSION>{}); }
	[[nodiscard]] inline constexpr bool Approx(const Vector& rhs, vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept
		{
			return (
				... && (Hydrogenium::abs((*this)[I] - rhs[I]) < tolerance)
				);
		}
		(std::make_index_sequence<DIMENSION>{});
	}

	// Mathmetical Operations
	[[nodiscard]] inline constexpr decltype(auto) operator-() const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Vector(-(*this)[I]...); }(std::make_index_sequence<DIMENSION>{}); }
	[[nodiscard]] inline constexpr decltype(auto) operator== (const Vector& v) const noexcept { return Approx(v); }
	[[nodiscard]] inline constexpr decltype(auto) operator<=> (const Vector& v) const noexcept { return LengthSquared() <=> v.LengthSquared(); }
	[[nodiscard]] inline constexpr decltype(auto) operator<=> (const real_t fl) const noexcept { return LengthSquared() <=> (fl * fl); }

	[[nodiscard]] inline constexpr decltype(auto) operator+(const Vector& v) const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Vector(((*this)[I] + v[I])...); }(std::make_index_sequence<DIMENSION>{}); }
	[[nodiscard]] inline constexpr decltype(auto) operator-(const Vector& v) const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Vector(((*this)[I] - v[I])...); }(std::make_index_sequence<DIMENSION>{}); }
	inline constexpr decltype(auto) operator+=(const Vector& v) noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { (((*this)[I] += v[I]), ...); }(std::make_index_sequence<DIMENSION>{}); }
	inline constexpr decltype(auto) operator-=(const Vector& v) noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { (((*this)[I] -= v[I]), ...); }(std::make_index_sequence<DIMENSION>{}); }

	[[nodiscard]] inline constexpr decltype(auto) operator*(real_t fl) const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Vector(((*this)[I] * fl)...); }(std::make_index_sequence<DIMENSION>{}); }
	[[nodiscard]] inline constexpr decltype(auto) operator/(real_t fl) const noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return Vector(((*this)[I] / fl)...); }(std::make_index_sequence<DIMENSION>{}); }
	inline constexpr decltype(auto) operator*=(real_t fl) noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { (((*this)[I] *= fl), ...); }(std::make_index_sequence<DIMENSION>{}); }
	inline constexpr decltype(auto) operator/=(real_t fl) noexcept { return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { (((*this)[I] /= fl), ...); }(std::make_index_sequence<DIMENSION>{}); }

	// Special status
	[[nodiscard]] static consteval Vector Zero() noexcept { return Vector(); }
	[[nodiscard]] static consteval Vector I() noexcept requires(DIMENSION >= 1) { return Vector(1); }
	[[nodiscard]] static consteval Vector J() noexcept requires(DIMENSION >= 2) { return Vector(0, 1); }
	[[nodiscard]] static consteval Vector K() noexcept requires(DIMENSION >= 3) { return Vector(0, 0, 1); }
	[[nodiscard]] static consteval Vector Up(void) noexcept requires(DIMENSION >= 3) { return Vector(0, 0, 1); }
	[[nodiscard]] static consteval Vector Down(void) noexcept requires(DIMENSION >= 3) { return Vector(0, 0, -1); }
	[[nodiscard]] static consteval Vector Right(void) noexcept requires(DIMENSION >= 2) { return Vector(0, -1); }
	[[nodiscard]] static consteval Vector Left(void) noexcept requires(DIMENSION >= 2) { return Vector(0, 1); }
	[[nodiscard]] static consteval Vector Front(void) noexcept requires(DIMENSION >= 1) { return Vector(1); }
	[[nodiscard]] static consteval Vector Back(void) noexcept requires(DIMENSION >= 1) { return Vector(-1); }

	//
	inline constexpr void Clear() noexcept { this->fill(0); }
	inline constexpr void CopyToRange(std::ranges::output_range<vec_t> auto&& rgObj) const noexcept { for (auto&& [Out, Comp] : std::views::zip(rgObj, *this)) Out = static_cast<std::remove_cvref_t<decltype(Out)>>(Comp); }
	template <size_t NEW_DIM> [[nodiscard]] inline constexpr Vector<NEW_DIM> Cast() const noexcept { return Vector<NEW_DIM>(*this | std::views::all); }

	// Conversion
	explicit inline constexpr operator vec_t* () noexcept { return &(*this)[0]; } // Vectors will now automatically convert to float * when needed
	explicit inline constexpr operator const vec_t* () const noexcept { return &(*this)[0]; } // Vectors will now automatically convert to float * when needed

	explicit inline constexpr operator bool() const noexcept { return !IsZero(); }	// Can be placed in if() now.
	explicit inline constexpr operator vec_t() const noexcept { return (vec_t)Length(); }
	explicit inline constexpr operator real_t() const noexcept { return Length(); }

	// Linear Algebra (For 2D Vectors)
	// Rotate in counter-clockwise. Angle is in degree.
	inline constexpr Vector<2> Rotate(const real_t angle) const noexcept requires(DIMENSION == 2)
	{
		const auto a = (angle * std::numbers::pi / 180.0);
		const auto c = gcem::cos(a);
		const auto s = gcem::sin(a);

		return Vector<2>(
			c * x - s * y,
			s * x + c * y
		);
	}

	// Angle with Vector2::I. Return in degree.
	inline constexpr auto Angle(void) const noexcept requires(DIMENSION == 2)
	{
		return gcem::atan2(y, x) * 180.0 / std::numbers::pi;
	}

	// Linear Algebra (Euler-3D Vector)
	inline constexpr Angles VectorAngles(void) const noexcept requires(DIMENSION == 3);
	inline constexpr real_t Pitch(void) const noexcept requires(DIMENSION == 3);
	inline constexpr real_t Yaw(void) const noexcept requires(DIMENSION == 3);
	static consteval real_t Roll(void) noexcept requires(DIMENSION == 3) { return 0; }	// It doesn't make any sense to ask the roll angle from a directional vector.
	inline constexpr Vector<3> RotateX(const real_t angle) const noexcept requires(DIMENSION == 3);
	inline constexpr Vector<3> RotateY(const real_t angle) const noexcept requires(DIMENSION == 3);
	inline constexpr Vector<3> RotateZ(const real_t angle) const noexcept requires(DIMENSION == 3);

	// 
	inline constexpr vec_t _Impl_GetX() const noexcept requires(DIMENSION >= 1) { return (*this)[0]; }
	inline constexpr void _Impl_SetX(const real_t val) noexcept requires(DIMENSION >= 1) { (*this)[0] = static_cast<vec_t>(val); }
	inline constexpr vec_t _Impl_GetY() const noexcept requires(DIMENSION >= 2) { return (*this)[1]; }
	inline constexpr void _Impl_SetY(const real_t val) noexcept requires(DIMENSION >= 2) { (*this)[1] = static_cast<vec_t>(val); }
	inline constexpr vec_t _Impl_GetZ() const noexcept requires(DIMENSION >= 3) { return (*this)[2]; }
	inline constexpr void _Impl_SetZ(const real_t val) noexcept requires(DIMENSION >= 3) { (*this)[2] = static_cast<vec_t>(val); }

	__declspec(property(get = _Impl_GetX, put = _Impl_SetX)) vec_t x;
	__declspec(property(get = _Impl_GetY, put = _Impl_SetY)) vec_t y;
	__declspec(property(get = _Impl_GetZ, put = _Impl_SetZ)) vec_t z;
};

export using Vector2 = Vector<2>;
export using Vector3 = Vector<3>;

export template <size_t DIMENSION>
constexpr real_t DotProduct(const Vector<DIMENSION>& a, const Vector<DIMENSION>& b) noexcept
{
	return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept { return (... + (a[I] * b[I])); }(std::make_index_sequence<DIMENSION>{});
}

export
constexpr Vector3 CrossProduct(const Vector3& a, const Vector3& b) noexcept
{
	return Vector3(
		a.y * b.z - a.z * b.y,
		a.z * b.x - a.x * b.z,
		a.x * b.y - a.y * b.x
	);
}

export
constexpr Vector3 CrossProduct(const Vector2& a, const Vector2& b) noexcept
{
	return Vector3(
		0,
		0,
		a.x * b.y - a.y * b.x
	);
}

export template <size_t DIMENSION>
constexpr Vector<DIMENSION> operator*(real_t fl, const Vector<DIMENSION>& v) noexcept
{
	return v * fl;
}

export template <size_t DIMENSION>
constexpr decltype(auto) operator^(const Vector<DIMENSION>& lhs, const Vector<DIMENSION>& rhs) noexcept	// Get the angle between two vectors. Returns an angle in degree.
{
	const real_t length_ab = Hydrogenium::sqrt(lhs.LengthSquared() * rhs.LengthSquared());	// sqrt(a) * sqrt(b) == sqrt(a*b)

	if (length_ab < std::numeric_limits<real_t>::epsilon())
		return (real_t)0;

	return gcem::acos(DotProduct(lhs, rhs) / length_ab) * (180.0 / std::numbers::pi);
}

#ifdef _IOSTREAM_
export template <size_t DIMENSION>
std::ostream& operator<<(std::ostream& o, const Vector<DIMENSION>& v) noexcept
{
	std::string s{"["};
	for (auto&& fl : v)
		s += std::to_string(fl) + ", ";

	s.erase(s.end() - 3);
	s.push_back(']');

	o << s;
	return o;
}
#endif // _IOSTREAM_

export
struct Angles : std::array<vec_t, 3>	// same data-layout as engine's vec3_t
{
	using super = std::array<vec_t, 3>;

	// Construction/destruction
	constexpr Angles(void) noexcept = default;	// == Foward()
	constexpr Angles(real_t P, real_t Y, real_t R) noexcept : super{ (vec_t)P, (vec_t)Y, (vec_t)R } {}
	constexpr Angles(const Angles& rhs) noexcept = default;

	// Special status
	static inline consteval Angles Upwards(void) noexcept { return Angles(90, 0, 0); }
	static inline consteval Angles Downwards(void) noexcept { return Angles(-90, 0, 0); }
	static inline consteval Angles Rightward(void) noexcept { return Angles(0, 270, 0); }
	static inline consteval Angles Leftward(void) noexcept { return Angles(0, 90, 0); }
	static inline consteval Angles Forward(void) noexcept { return Angles(0, 0, 0); }
	static inline consteval Angles Rearward(void) noexcept { return Angles(0, 180, 0); }

	// Operators
	inline constexpr Angles operator-(void) const noexcept { return Angles(-pitch, -yaw, -roll); }
	inline constexpr Angles operator+(const Angles& a) const noexcept { return Angles(pitch + a.pitch, yaw + a.yaw, roll + a.roll); }
	inline constexpr Angles operator-(const Angles& a) const noexcept { return Angles(pitch - a.pitch, yaw - a.yaw, roll - a.roll); }
	inline constexpr Angles operator*(real_t fl) const noexcept { return Angles(pitch * fl, yaw * fl, roll * fl); }
	inline constexpr Angles operator/(real_t fl) const noexcept { fl = 1 / fl; return Angles(pitch * fl, yaw * fl, roll * fl); }
	inline constexpr Angles& operator+=(const Angles& a) noexcept { pitch += a.pitch; yaw += a.yaw; roll += a.roll; return *this; }
	inline constexpr Angles& operator-=(const Angles& a) noexcept { pitch -= a.pitch; yaw -= a.yaw; roll -= a.roll; return *this; }
	inline constexpr Angles& operator*=(real_t fl) noexcept { pitch *= fl; yaw *= fl; roll *= fl; return *this; }
	inline constexpr Angles& operator/=(real_t fl) noexcept { pitch /= fl; yaw /= fl; roll /= fl; return *this; }

	// Conversion
	explicit inline constexpr operator float* () noexcept { return this->data(); } // Vectors will now automatically convert to float * when needed
	explicit inline constexpr operator const float* () const noexcept { return std::ranges::cdata(*this); } // Vectors will now automatically convert to float * when needed

	// Linear Algebra
	inline constexpr Angles Rationalize(void) const noexcept	// One must call this function before conver to quaternion.
	{
		Angles ret{ *this };

		for (int i = 0; i < 3; ++i)
		{
			while (ret[i] > 360)
				ret[i] -= 360;
			while (ret[i] < -360)
				ret[i] += 360;

			while (ret[i] > 180)
				ret[i] -= 360;
			while (ret[i] < -180)
				ret[i] += 360;
		}

		if (ret.pitch > 90)
		{
			ret.pitch = -(180.f - ret.pitch);
			ret.roll += 180.f;
		}
		else if (ret.pitch < -90)
		{
			ret.pitch = ret.pitch + 180.f;
			ret.roll += 180.f;
		}

		ret.pitch = std::clamp(ret.pitch, -89.99f, 89.99f);	// LUNA: due to the nature of Quaternion, the pitch must fall in the range of (-90, 90)
		//ret.yaw = std::clamp(ret.yaw, -179.99f, 179.99f);	// whereas roll and yaw could be [-180, 180]
		//ret.roll = std::clamp(ret.roll, -179.99f, 179.99f);

		return ret;
	}
	inline constexpr std::tuple<Vector3, Vector3, Vector3> AngleVectors() const noexcept
	{
		const auto sp = gcem::sin(pitch * deg_to_rad), sy = gcem::sin(yaw * deg_to_rad), sr = gcem::sin(roll * deg_to_rad);
		const auto cp = gcem::cos(pitch * deg_to_rad), cy = gcem::cos(yaw * deg_to_rad), cr = gcem::cos(roll * deg_to_rad);

		return std::make_tuple(

			// Forward
			Vector3(
				cp * cy,	// x
				cp * sy,	// y
				-sp		// z
			),

			// Right
			Vector3(
				-(sr * sp * cy) + cr * sy,	// x
				-(sr * sp * sy) - cr * cy,	// y
				-(sr * cp)		// z
			),

			// Up
			Vector3(
				cr * sp * cy + sr * sy,	// x
				cr * sp * sy - sr * cy,	// y
				cr * cp		// z
			)
		);
	}
	static inline constexpr Angles VectorsAngles(const Vector3& vecForward, const Vector3& vecRight, const Vector3& vecUp) noexcept // Unify three vectors into one Eular angles.
	{
		Angles ret;
		const auto p = -gcem::asin<real_t>(vecForward.z);
		auto cp = gcem::cos<real_t>(p);
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
	inline constexpr Vector3 Up(void) const noexcept
	{
		const auto sp = gcem::sin(pitch * deg_to_rad), sy = gcem::sin(yaw * deg_to_rad), sr = gcem::sin(roll * deg_to_rad);
		const auto cp = gcem::cos(pitch * deg_to_rad), cy = gcem::cos(yaw * deg_to_rad), cr = gcem::cos(roll * deg_to_rad);

		return Vector3(
			cr * sp * cy + sr * sy,	// x
			cr * sp * sy - sr * cy,	// y
			cr * cp		// z
		);
	}
	inline constexpr Vector3 Down(void) const noexcept
	{
		return -Up();
	}
	inline constexpr Vector3 Right(void) const noexcept
	{
		const auto sp = gcem::sin(pitch * deg_to_rad), sy = gcem::sin(yaw * deg_to_rad), sr = gcem::sin(roll * deg_to_rad);
		const auto cp = gcem::cos(pitch * deg_to_rad), cy = gcem::cos(yaw * deg_to_rad), cr = gcem::cos(roll * deg_to_rad);

		return Vector3(
			-(sr * sp * cy) + cr * sy,	// x
			-(sr * sp * sy) - cr * cy,	// y
			-(sr * cp)		// z
		);
	}
	inline constexpr Vector3 Left(void) const noexcept
	{
		return -Right();
	}
	inline constexpr Vector3 Front(void) const noexcept
	{
		const auto sp = gcem::sin(pitch * deg_to_rad), sy = gcem::sin(yaw * deg_to_rad);
		const auto cp = gcem::cos(pitch * deg_to_rad), cy = gcem::cos(yaw * deg_to_rad);

		return Vector3(
			cp * cy,	// x
			cp * sy,	// y
			-sp		// z
		);
	}
	inline constexpr Vector3 Back(void) const noexcept
	{
		return -Front();
	}

	// Methods
	[[nodiscard]] inline constexpr bool Approx(const Angles& rhs, const vec_t tolerance = VEC_EPSILON) const noexcept
	{
		return
			Hydrogenium::abs(pitch - rhs.pitch) < tolerance &&
			Hydrogenium::abs(yaw - rhs.yaw) < tolerance &&
			Hydrogenium::abs(roll - rhs.roll) < tolerance;
	}

	// Members
	static inline constexpr auto deg_to_rad = std::numbers::pi / 180.0;
	static inline constexpr auto rad_to_deg = 180.0 / std::numbers::pi;

	inline constexpr vec_t _Impl_GetP(void) const noexcept { return (*this)[0]; }
	inline constexpr void _Impl_SetP(const real_t fl) noexcept { (*this)[0] = static_cast<vec_t>(fl); }
	inline constexpr vec_t _Impl_GetY(void) const noexcept { return (*this)[1]; }
	inline constexpr void _Impl_SetY(const real_t fl) noexcept { (*this)[1] = static_cast<vec_t>(fl); }
	inline constexpr vec_t _Impl_GetR(void) const noexcept { return (*this)[2]; }
	inline constexpr void _Impl_SetR(const real_t fl) noexcept { (*this)[2] = static_cast<vec_t>(fl); }
	inline constexpr vec_t _Impl_GetRP(void) const noexcept { return static_cast<vec_t>((*this)[0] * deg_to_rad); }
	inline constexpr void _Impl_SetRP(const real_t fl) noexcept { (*this)[0] = static_cast<vec_t>(fl * rad_to_deg); }
	inline constexpr vec_t _Impl_GetRY(void) const noexcept { return static_cast<vec_t>((*this)[1] * deg_to_rad); }
	inline constexpr void _Impl_SetRY(const real_t fl) noexcept { (*this)[1] = static_cast<vec_t>(fl * rad_to_deg); }
	inline constexpr vec_t _Impl_GetRR(void) const noexcept { return static_cast<vec_t>((*this)[2] * deg_to_rad); }
	inline constexpr void _Impl_SetRR(const real_t fl) noexcept { (*this)[2] = static_cast<vec_t>(fl * rad_to_deg); }

	__declspec(property(get = _Impl_GetP, put = _Impl_SetP)) vec_t pitch;
	__declspec(property(get = _Impl_GetY, put = _Impl_SetY)) vec_t yaw;
	__declspec(property(get = _Impl_GetR, put = _Impl_SetR)) vec_t roll;
	__declspec(property(get = _Impl_GetRP, put = _Impl_SetRP)) vec_t rad_pitch;
	__declspec(property(get = _Impl_GetRY, put = _Impl_SetRY)) vec_t rad_yaw;
	__declspec(property(get = _Impl_GetRR, put = _Impl_SetRR)) vec_t rad_roll;
};

template <size_t DIMENSION>
inline constexpr Angles Vector<DIMENSION>::VectorAngles(void) const noexcept requires(DIMENSION == 3)
{
	Angles angles{};

	if (y == 0 && x == 0)
	{
		angles.yaw = 0;
		if (z > 0)
			angles.pitch = 90;
		else
			angles.pitch = -90;
	}
	else
	{
		angles.yaw = static_cast<float>(gcem::atan2<real_t, real_t>(y, x) * Angles::rad_to_deg);
		if (angles.yaw < 0)
			angles.yaw += 360;

		angles.pitch = static_cast<float>(gcem::atan2<real_t, real_t>(z, Length<2>()) * Angles::rad_to_deg);
		if (angles.pitch < 0)
			angles.pitch += 360;
	}

	return angles;
}

template <size_t DIMENSION>
inline constexpr real_t Vector<DIMENSION>::Pitch(void) const noexcept requires(DIMENSION == 3)
{
	if (y == 0 && x == 0)
	{
		return (z > 0) ? 90 : -90;
	}
	else
	{
		return gcem::atan2<real_t, real_t>(z, Length<2>()) * Angles::rad_to_deg;
	}
}

template <size_t DIMENSION>
inline constexpr real_t Vector<DIMENSION>::Yaw(void) const noexcept requires(DIMENSION == 3)
{
	if (y == 0 && x == 0)
	{
		return 0;
	}
	else
	{
		return gcem::atan2<real_t, real_t>(y, x) * Angles::rad_to_deg;
	}
}

template <size_t DIMENSION>
constexpr Vector<3> Vector<DIMENSION>::RotateX(const real_t angle) const noexcept requires(DIMENSION == 3)
{
	const auto a = (angle * Angles::deg_to_rad);
	const auto c = gcem::cos(a);
	const auto s = gcem::sin(a);
	return Vector<3>(
		x,
		c * y - s * z,
		s * y + c * z
	);
}

template <size_t DIMENSION>
constexpr Vector<3> Vector<DIMENSION>::RotateY(const real_t angle) const noexcept requires(DIMENSION == 3)
{
	const auto a = (angle * Angles::deg_to_rad);
	const auto c = gcem::cos(a);
	const auto s = gcem::sin(a);
	return Vector<3>(
		c * x + s * z,
		y,
		-s * x + c * z
	);
}

template <size_t DIMENSION>
constexpr Vector<3> Vector<DIMENSION>::RotateZ(const real_t angle) const noexcept requires(DIMENSION == 3)
{
	const auto a = (angle * Angles::deg_to_rad);
	const auto c = gcem::cos(a);
	const auto s = gcem::sin(a);
	return Vector<3>(
		c * x - s * y,
		s * x + c * y,
		z
	);
}

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
	explicit constexpr Matrix(std::ranges::input_range auto&& rgObj) noexcept requires(std::is_convertible_v<std::ranges::range_value_t<decltype(rgObj)>, double>) : _data{}
	{
		for (auto&& [Input, Cell] : std::views::zip(rgObj, std::views::join(_data)))
		{
			Cell = static_cast<mxs_t>(Input);
		}
	}
	template <size_t VECDIM> explicit constexpr Matrix(Vector<VECDIM> const& v) noexcept requires(ROWS >= VECDIM && COLUMNS == 1U) : _data{}
	{
		for (size_t c = 0; c < VECDIM; ++c)
		{
			_data[c][0] = v[c];
		}

		if constexpr (ROWS > VECDIM)
		{
			_data[ROWS - 1U][0] = 1;	// For example, if you wish Vector2(x, y) transcript to matrix<4, 1>, it must be [x, y, 0, 1].
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
	static constexpr decltype(auto) Rotation(const Angles& vecEulerAngles) noexcept { return Rotation(vecEulerAngles.yaw, vecEulerAngles.pitch, vecEulerAngles.roll); }
	static constexpr decltype(auto) Rotation(const Vector3& vecAxis, double degree) noexcept	// Axis must be a unit vector. In counter-clockwise. Quaternion is recommended in this case.
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
	template <size_t VECDIM> 
	constexpr decltype(auto) operator*(const Vector<VECDIM>& v) const noexcept requires(COLUMNS >= VECDIM)
	{
		Matrix<COLUMNS, 1> matrixlise_v(v);

		if constexpr (COLUMNS > VECDIM)
		{
			for (size_t i = VECDIM; i < COLUMNS; ++i)
			{
				matrixlise_v[i][0] = 1;	// Fill the rest part with a dummy 1. NOT A ZERO!
			}
		}

		auto const result = *this * matrixlise_v;

		return[&]<size_t... I>(std::index_sequence<I...>&&) constexpr noexcept
		{
			return Vector<VECDIM>(result[I][0]...);
		}
		(std::make_index_sequence<VECDIM>{});
	}
	constexpr Vector3 operator*(const Vector3& v) const noexcept requires(COLUMNS >= 3U)
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

		return Vector3(result[0][0], result[1][0], result[2][0]);
	}
	constexpr Matrix<3, 3> operator|(const Vector2& v) const noexcept requires(ROWS == 2U && COLUMNS == 2U)
	{
		return Matrix<3, 3>({
			{_data[0][0], _data[0][1], v.x},
			{_data[1][0], _data[1][1], v.y},
			{0, 0, 1}
		});
	}
	constexpr Matrix<4, 4> operator|(const Vector3& v) const noexcept requires(ROWS == 3U && COLUMNS == 3U)
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

		return[&]<size_t... I>(std::index_sequence<I...>&&) noexcept
		{
			// Vectors are written vertically in matrices.
			return Vector<ROWS>(_data[I][c]...);
		}
		(std::make_index_sequence<ROWS>{});
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
	[[nodiscard]] constexpr const_reference at(size_type pos) const	// #UPDATE_AT_CPP23 explicit this
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
	constexpr Quaternion(const Vector3& vecAxis, qtn_t degree) noexcept	// Axis must be a unit vector. In clockwise if works in right-handed coordinate system.
	{
		degree *= std::numbers::pi / 180.0;
		auto cosine = gcem::cos(0.5 * degree);
		auto sine = gcem::sin(0.5 * degree);

		a = cosine;
		b = vecAxis.x * sine;
		c = vecAxis.y * sine;
		d = vecAxis.z * sine;
	}
	constexpr Quaternion(const Vector3 &vecFrom, const Vector3 &vecTo) noexcept
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
	explicit constexpr Quaternion(const Angles& vecEulerAngles) : Quaternion(vecEulerAngles.yaw, vecEulerAngles.pitch, vecEulerAngles.roll) {}
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
	inline constexpr Vector3 Pure() const noexcept { return Vector3(b, c, d); }

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
	constexpr decltype(auto) operator*(const Vector3& v) const noexcept { return 2.0 * DotProduct(Pure(), v) * Pure() + (a * a - Pure().LengthSquared()) * v + 2.0 * a * CrossProduct(Pure(), v); }	// Rotate a vector by this quaternion.

	// Access
	constexpr qtn_t& operator[](size_t index) noexcept { assert(index < 4); return ((qtn_t*)(&a))[index]; }
	constexpr const qtn_t operator[](size_t index) const noexcept { assert(index < 4); return ((const qtn_t*)(&a))[index]; }

	// Conversion
	constexpr Angles Euler() const noexcept
	{
		Angles vecAngles;

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
