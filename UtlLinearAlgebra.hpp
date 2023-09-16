#pragma once

// C
#include <cassert>
#include <cmath>

// C++
#include <algorithm>
#include <array>
#include <concepts>
#include <limits>
#include <numbers>
#include <ranges>
#include <string>

namespace Hydrogenium
{
	__forceinline constexpr auto abs(auto val) noexcept { return val < 0 ? -val : val; }
}

namespace Hydrogenium
{
	using std::array;

	using vec_t = float;
	using real_t = std::conditional_t<sizeof(vec_t) <= 4, double, vec_t>;
	inline constexpr auto VEC_EPSILON = std::numeric_limits<vec_t>::epsilon();
	inline constexpr auto VEC_NAN = std::numeric_limits<vec_t>::quiet_NaN();
	inline constexpr auto VEC_INFINITY = std::numeric_limits<vec_t>::infinity();

	template <typename T>
	concept Arithmetic = std::is_arithmetic_v<T>;

	template <typename T>
	concept ArithmeticInputRange = std::ranges::input_range<T> && std::is_arithmetic_v<std::ranges::range_value_t<T>>;

	template <size_t DIMENSION>
	struct Vector
	{
		// Construction

		inline constexpr Vector(void) noexcept = default;
		inline constexpr Vector(Arithmetic auto... args) noexcept : m_components{ static_cast<vec_t>(args)... } {}

		explicit constexpr Vector(ArithmeticInputRange auto&& rng, Arithmetic auto... args) noexcept
		{
			for (auto&& [Ref, Val] : std::views::zip(m_components, rng))
				Ref = static_cast<vec_t>(Val);

			if constexpr (sizeof...(args) > 0)
			{
				if (auto const iSize = std::ranges::size(rng); iSize < DIMENSION)
				{
					array<vec_t, sizeof...(args)> arr{ static_cast<vec_t>(args)... };

					for (auto&& [Ref, Val] : std::views::zip(m_components | std::views::drop(iSize), arr))
						Ref = Val;
				}
			}
		}

		// Math

		template <size_t COMPONENTS = DIMENSION>
		[[nodiscard]] inline /*constexpr*/ real_t Length() const noexcept // #UPDATE_AT_CPP26_constexpr_math
		{
			return [&]<size_t... I>(std::index_sequence<I...>&&) noexcept
			{
				return std::sqrt(
					(... + ((real_t)m_components[I] * (real_t)m_components[I]))
				);
			}
			(std::make_index_sequence<COMPONENTS>{});
		}

		template <size_t COMPONENTS = DIMENSION>
		[[nodiscard]] inline constexpr real_t LengthSquared() const noexcept
		{
			return [&]<size_t... I>(std::index_sequence<I...>&&) noexcept
			{
				return (
					... + ((real_t)m_components[I] * (real_t)m_components[I])
					);
			}
			(std::make_index_sequence<COMPONENTS>{});
		}

		template <vec_t tolerance = VEC_EPSILON>
		[[nodiscard]] inline constexpr bool Approx(const Vector& rhs) const noexcept
		{
			return [&]<size_t... I>(std::index_sequence<I...>&&) noexcept
			{
				return (
					... && (Hydrogenium::abs(m_components[I] - rhs[I]) < tolerance)
					);
			}
			(std::make_index_sequence<DIMENSION>{});
		}

		// Operators

		[[nodiscard]] inline constexpr auto operator== (Vector const& rhs) const noexcept { return Approx(rhs); }
		[[nodiscard]] inline constexpr auto operator<=> (Vector const& rhs) const noexcept { return LengthSquared() <=> rhs.LengthSquared(); }
		[[nodiscard]] inline constexpr auto operator== (Arithmetic auto fl) const noexcept { assert(fl >= 0); if (fl < 0) return false; return Hydrogenium::abs(LengthSquared() - fl * fl) <= VEC_EPSILON; }
		[[nodiscard]] inline constexpr auto operator<=> (Arithmetic auto fl) const noexcept { assert(fl >= 0); if (fl < 0) fl = 0; return LengthSquared() <=> (fl * fl); }

		// Constants

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

		// STL property

		[[nodiscard]] __forceinline constexpr decltype(auto) operator[] (this auto&& self, std::integral auto idx) noexcept { return self.m_components[idx]; }

		[[nodiscard]] static consteval auto size(void) noexcept { return DIMENSION; }

		[[nodiscard]] inline constexpr auto cbegin(this auto&& self) noexcept { return self.m_components.cbegin(); }
		[[nodiscard]] inline constexpr auto begin(this auto&& self) noexcept { return self.m_components.begin(); }
		[[nodiscard]] inline constexpr auto rbegin(this auto&& self) noexcept { return self.m_components.rbegin(); }
		[[nodiscard]] inline constexpr auto crbegin(this auto&& self) noexcept { return self.m_components.crbegin(); }

		[[nodiscard]] inline constexpr auto cend(this auto&& self) noexcept { return self.m_components.cend(); }
		[[nodiscard]] inline constexpr auto end(this auto&& self) noexcept { return self.m_components.end(); }
		[[nodiscard]] inline constexpr auto rend(this auto&& self) noexcept { return self.m_components.rend(); }
		[[nodiscard]] inline constexpr auto crend(this auto&& self) noexcept { return self.m_components.crend(); }

		array<vec_t, DIMENSION> m_components{};
	};
}

using Vector2 = Hydrogenium::Vector<2>;
