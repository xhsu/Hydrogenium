/*
* Provide formatter for
* Vector2D
* Vector
* Quaternion
* Color4b
* Color4f
* 
* Nov 27 2021
*/

#include <format>
#include <string>

namespace std
{
	// Vector2D
	template<>
	struct formatter<Vector2D> : formatter<string>
	{
		auto format(const Vector2D& v, format_context& ctx) noexcept
		{
			return formatter<string>::format(std::format("{} {}", v.x, v.y), ctx);
		}
	};

	// Vector
	template<>
	struct formatter<Vector> : formatter<string>
	{
		auto format(const Vector& v, format_context& ctx) noexcept
		{
			return formatter<string>::format(std::format("{} {} {}", v.x, v.y, v.z), ctx);
		}
	};

	// Quaternion
	template<>
	struct formatter<Quaternion> : formatter<string>
	{
		auto format(const Quaternion& q, format_context& ctx) noexcept
		{
			return formatter<string>::format(std::format("{} {} {} {}", q.a, q.b, q.c, q.d), ctx);
		}
	};

	// Color4b
	template<>
	struct formatter<Color4b> : formatter<string>
	{
		auto format(const Color4b& c, format_context& ctx) noexcept
		{
			if (c[3] == 0)
				return formatter<string>::format(std::format("{} {} {}", (int)c[0], (int)c[1], (int)c[2]), ctx);
			else
				return formatter<string>::format(std::format("{} {} {} {}", (int)c[0], (int)c[1], (int)c[2], (int)c[3]), ctx);
		}
	};

	// Color4f
	template<>
	struct formatter<Color4f> : formatter<string>
	{
		auto format(const Color4f& c, format_context& ctx) noexcept
		{
			if (c[3] <= std::numeric_limits<std::decay_t<decltype(c[3])>>::epsilon())
				return formatter<string>::format(std::format("{} {} {}", c[0], c[1], c[2]), ctx);
			else
				return formatter<string>::format(std::format("{} {} {} {}", c[0], c[1], c[2], c[3]), ctx);
		}
	};
}
