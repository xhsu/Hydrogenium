#include "../UtlPatternMatching.hpp"

#include <assert.h>

#include <expected>
#include <memory>
#include <optional>
#include <string_view>
#include <map>
#include <array>

using namespace std;
using namespace std::literals;

struct Base { virtual ~Base() = default; int m_i{}; };
struct Derived : Base { virtual ~Derived() = default; double m_fl{}; };
struct NotRelated final { virtual ~NotRelated() = default; bool m_b{}; };

static_assert(std::is_base_of_v<Base, Derived>);
static_assert(!std::is_base_of_v<Base*, Derived*>);
static_assert(!std::is_base_of_v<Base&, Derived&>);
static_assert(!std::is_base_of_v<Base&&, Derived&&>);


void unit_test_stl_types() noexcept
{
	variant<monostate, int, double, string_view> v{ 1 };
	assert(v / is<int>);
	assert(v / is_not<double>);
	assert(v / is_not<string_view>);
	static_assert(std::same_as<decltype(v / as<int>), int&>);

	(v / as<int>)++;
	assert(v / is<int>);
	assert(v / is_not<double>);
	assert(v / is_not<string_view>);
	assert(v / as<int> == 2);
	v = 2.0;
	assert(v / is_not<int>);
	assert(v / is<double>);
	assert(v / is_not<string_view>);
	assert(v / as<double> == 2.0);

	auto const& ref_v = v;
	assert(ref_v / is_not<int>);
	assert(ref_v / is<double>);
	assert(ref_v / is_not<string_view>);
	static_assert(std::same_as<decltype(ref_v / as<double>), double const&>);

	v = monostate{};
	assert(v / is_null);
	assert(ref_v / is_null);

	any a{ 1 };
	assert(a / is<int>);
	assert(a / is<int&>);
	assert(a / is_not<float>);
	assert(a / is_not<float&>);
	static_assert(std::same_as<decltype(a / as<int>), int>);
	static_assert(std::same_as<decltype(a / as<int&>), int&>);

	auto const& ref_a = a;
	assert(ref_a / is<int const>);
	assert(ref_a / is<int const&>);
	assert(ref_a / is_not<float const>);
	assert(ref_a / is_not<float const&>);
	static_assert(std::same_as<decltype(ref_a / as<int const&>), int const&>);

	a.reset();
	assert(a / is_null);
	assert(ref_a / is_null);

	expected<int, double> e{ 1 };
	assert(e / is<int>);
	assert(e / is_not<double>);
	static_assert(std::same_as<decltype(e / as<int>), int&>);
	static_assert(std::same_as<decltype(e / as<double>), double&>);

	auto const& ref_e = e;
	assert(ref_e / is<int const>);
	assert(ref_e / is_not<double const>);
	static_assert(std::same_as<decltype(ref_e / as<int>), int const&>);
	static_assert(std::same_as<decltype(ref_e / as<double>), double const&>);

	e = std::unexpected(1.0);
	assert(e / is_not<int>);
	assert(e / is<double>);
	assert(ref_e / is_not<int const>);
	assert(ref_e / is<double const>);

	auto l = []() -> expected<void, int> { return {}; };
	assert(l() / is_null);
	assert(!(e / is_null));

	optional o{ 1 };
	assert(o / is<int>);
	assert(o / is_not<std::nullopt_t>);
	static_assert(std::same_as<decltype(o / as<int>), int&>);

	auto const& ref_o = o;
	assert(ref_o / is<int const>);
	assert(ref_o / is_not<std::nullopt_t>);
	static_assert(std::same_as<decltype(ref_o / as<int>), int const&>);

	o = std::nullopt;
	assert(o / is<std::nullopt_t>);
	assert(ref_o / is<std::nullopt_t>);
	assert(o / is_not<int>);
	assert(ref_o / is_not<int>);
	assert(o / is_null);
	assert(ref_o / is_null);

	future<int> f = std::async(std::launch::async, [] { this_thread::sleep_for(0.1s); return 8; });
	assert(f / is_null);
	f.wait();
	assert(f / is<int>);
	assert(f / is_not<float>);
	static_assert(std::same_as<decltype(f / as<int>), int>);
	assert(f / as<int> == 8);
}

void unit_test_ranges() noexcept
{
	string s1{ "hello" };
	string_view sV{ "world" };

	assert(s1 / is_not_null);
	assert(sV / is_not_null);

	s1 = "";
	sV = "";

	assert(s1 / is_null);
	assert(sV / is_null);

	vector vec{ 0, 1, 2 };
	map<string_view, int, less<>> m{ {"hello", 1}, {"world", 2} };
	array arr{ 2.f, 3.f, 4.f };

	assert(vec / is_not_null);
	assert(m / is_not_null);
	assert(arr / is_not_null);

	vec.clear();
	m.clear();
	arr.fill(0);

	assert(vec / is_null);
	assert(m / is_null);
	assert(arr / is_not_null);

	array<int, 0> no_numbers;
	assert(no_numbers / is_null);
}

void unit_test_native_ptr() noexcept
{
	using B = Base;
	using D = Derived;
	using N = NotRelated;

	// derived

	B* p = new D;

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);

	assert(p / is<D*>);
	assert(p / as<D*> != nullptr);

	assert(*p / is<B&>);
	assert(*p / is<D&>);

	assert(p / is_not_null);

	static_assert(std::same_as<decltype(*p / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p / as<D&>), D&>);
	delete p;

	// base

	p = new B;

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);

	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	assert(*p / is<B&>);
	assert(!(*p / is<D&>));

	delete p;

	// B*, nullptr

	p = nullptr;

	assert(!(p / is<B*>));
	assert(p / as<B*> == nullptr);

	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	assert(p / is_null);

	// D*

	D* p2{ new D };

	assert(p2 / is<B*>);
	assert(p2 / as<B*> != nullptr);

	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);

	assert(*p2 / is<B&>);
	assert(*p2 / is<D&>);

	assert(p2 / is_not_null);

	static_assert(std::same_as<decltype(*p2 / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p2 / as<D&>), D&>);
	delete p2;

	// D*, nullptr

	p2 = nullptr;

	assert(!(p2 / is<B*>));
	assert(p2 / as<B*> == nullptr);

	assert(!(p2 / is<D*>));
	assert(p2 / as<D*> == nullptr);

	assert(p2 / is_null);
}

void unit_test_unique_ptr() noexcept
{
	using B = Base;
	using D = Derived;
	using N = NotRelated;

	// derived

	unique_ptr<B> p{ new D };

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);
	assert(p / is<std::nullptr_t> && !p && p / is_null);

	p = make_unique<D>();
	assert(p / is<D*>);
	assert(p / as<D*> != nullptr);
	assert(p / is<std::nullptr_t> && !p && p / is_null);

	p = make_unique<D>();
	assert(*p / is<B&>);
	assert(*p / is<D&>);

	static_assert(std::same_as<decltype(*p / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p / as<D&>), D&>);

	// base

	p = make_unique<B>();

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);
	assert(p / is<std::nullptr_t> && !p && p / is_null);

	p = make_unique<B>();
	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	assert(*p / is<B&>);
	assert(!(*p / is<D&>));

	// B*, nullptr

	p = nullptr;

	assert(!(p / is<B*>));
	assert(p / as<B*> == nullptr);

	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	// D*

	unique_ptr<D> p2{ new D };

	assert(p2 / is<B*>);
	assert(p2 / as<B*> != nullptr);
	assert(p2 / is<std::nullptr_t> && !p2 && p2 / is_null);

	p2 = make_unique<D>();
	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);
	assert(p2 / is<std::nullptr_t> && !p2 && p2 / is_null);

	p2 = make_unique<D>();
	assert(*p2 / is<B&>);
	assert(*p2 / is<D&>);

	static_assert(std::same_as<decltype(*p2 / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p2 / as<D&>), D&>);

	// D*, nullptr

	p2 = nullptr;

	assert(!(p2 / is<B*>));
	assert(p2 / as<B*> == nullptr);

	assert(!(p2 / is<D*>));
	assert(p2 / as<D*> == nullptr);
}

void unit_test_shared_ptr() noexcept
{
	using B = Base;
	using D = Derived;
	using N = NotRelated;

	// derived

	shared_ptr<B> p = std::make_shared<D>();

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);

	assert(p / is<D*>);
	assert(p / as<D*> != nullptr);

	assert(*p / is<B&>);
	assert(*p / is<D&>);

	assert(p / is_not_null);

	static_assert(std::same_as<decltype(*p / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p / as<D&>), D&>);

	// base

	p = make_shared<B>();

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);

	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	assert(*p / is<B&>);
	assert(!(*p / is<D&>));

	// B*, nullptr

	p = nullptr;

	assert(!(p / is<B*>));
	assert(p / as<B*> == nullptr);

	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	assert(p / is_null);

	// D*

	auto p2 = make_shared<D>();

	assert(p2 / is<B*>);
	assert(p2 / as<B*> != nullptr);

	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);

	assert(*p2 / is<B&>);
	assert(*p2 / is<D&>);

	assert(p2 / is_not_null);

	static_assert(std::same_as<decltype(*p2 / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p2 / as<D&>), D&>);

	// D*, nullptr

	p2 = nullptr;

	assert(!(p2 / is<B*>));
	assert(p2 / as<B*> == nullptr);

	assert(!(p2 / is<D*>));
	assert(p2 / as<D*> == nullptr);

	assert(p2 / is_null);
}

void unit_test_weak_ptr() noexcept
{
	using B = Base;
	using D = Derived;
	using N = NotRelated;

	auto pb = make_shared<B>();
	auto pd = make_shared<D>();

	// derived

	weak_ptr<B> p = pd;

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);

	assert(p / is<D*>);
	assert(p / as<D*> != nullptr);

	//assert(*p / is<B&>);
	//assert(*p / is<D&>);

	assert(p / is_not_null);

	//static_assert(std::same_as<decltype(*p / as<B&>), B&>);
	//static_assert(std::same_as<decltype(*p / as<D&>), D&>);

	// base

	p = pb;

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);

	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	//assert(*p / is<B&>);
	//assert(!(*p / is<D&>));

	// B*, nullptr

	p.reset();

	assert(!(p / is<B*>));
	assert(p / as<B*> == nullptr);

	assert(!(p / is<D*>));
	assert(p / as<D*> == nullptr);

	assert(p / is_null);

	// D*

	weak_ptr<D> p2 = pd;

	assert(p2 / is<B*>);
	assert(p2 / as<B*> != nullptr);

	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);

	//assert(*p2 / is<B&>);
	//assert(*p2 / is<D&>);

	assert(p2 / is_not_null);

	//static_assert(std::same_as<decltype(*p2 / as<B&>), B&>);
	//static_assert(std::same_as<decltype(*p2 / as<D&>), D&>);

	// D*, nullptr

	p2.reset();

	assert(!(p2 / is<B*>));
	assert(p2 / as<B*> == nullptr);

	assert(!(p2 / is<D*>));
	assert(p2 / as<D*> == nullptr);

	assert(p2 / is_null);
}


int main() noexcept
{
	unit_test_native_ptr();
	unit_test_unique_ptr();
	unit_test_shared_ptr();
	unit_test_weak_ptr();
	unit_test_stl_types();
	unit_test_ranges();
}
