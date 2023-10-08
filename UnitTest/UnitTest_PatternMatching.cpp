#include "../UtlPatternMatching.hpp"

#include <assert.h>
#include <expected>
#include <optional>
#include <string_view>

using namespace std;
using namespace std::literals;

struct Base { virtual ~Base() = default; int m_i{}; };
struct Derived : Base { virtual ~Derived() = default; double m_fl{}; };

static_assert(std::is_base_of_v<Base, Derived>);
static_assert(!std::is_base_of_v<Base*, Derived*>);
static_assert(!std::is_base_of_v<Base&, Derived&>);
static_assert(!std::is_base_of_v<Base&&, Derived&&>);


void unit_test_pattern_matching() noexcept
{
	variant<int, double, string_view> v{ 1 };
	static_assert(std::same_as<decltype(v / as<int>), int&>);

	(v / as<int>)++;
	assert(v / as<int> == 2);
	v = 2.0;
	assert(v / as<double> == 2.0);

	auto const& ref_v = v;
	static_assert(std::same_as<decltype(ref_v / as<int>), int const&>);

	any a{ 1 };
	static_assert(std::same_as<decltype(a / as<int>), int&>);

	auto const& ref_a = a;
	static_assert(std::same_as<decltype(ref_a / as<int const>), int const&>);

	expected<int, double> e{ 1 };
	static_assert(std::same_as<decltype(e / as<int>), int&>);
	static_assert(std::same_as<decltype(e / as<double>), double&>);

	auto const& ref_e = e;
	static_assert(std::same_as<decltype(ref_e / as<int>), int const&>);
	static_assert(std::same_as<decltype(ref_e / as<double>), double const&>);

	optional o{ 1 };
	static_assert(std::same_as<decltype(o / as<int>), int&>);

	auto const& ref_o = o;
	static_assert(std::same_as<decltype(ref_o / as<int>), int const&>);

	Base* p = new Derived;
	assert(p / as<Derived*> != nullptr);
	assert(p / is<Base*>);
	assert(p / is<Derived*>);
	assert(*p / is<Derived&>);
	static_assert(std::same_as<decltype(*p / as<Derived&>), Derived&>);
	delete p;

	p = new Base;
	assert(p / as<Derived*> == nullptr);
	assert(!(*p / is<Derived&>));
	//try { *p / as<Derived&>; } catch (...) { fmt::print("*p / as<Derived&> => error catched!\n"); }
	delete p;
	p = nullptr;

	Derived* p2{};
	assert(p2 / is<Base*>);
}
