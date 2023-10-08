#include "../UtlPatternMatching.hpp"

#include <assert.h>
#include <expected>
#include <memory>
#include <optional>
#include <string_view>

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
	variant<int, double, string_view> v{ 1 };
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

	future<int> f = std::async(std::launch::async, [] { return 8; });
	f.wait();
	assert(f / is<int>);
	assert(f / is_not<float>);
	static_assert(std::same_as<decltype(f / as<int>), int>);
	assert(f / as<int> == 8);
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

	// D*

	D* p2{ new D };

	assert(p2 / is<B*>);
	assert(p2 / as<B*> != nullptr);

	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);

	assert(*p2 / is<B&>);
	assert(*p2 / is<D&>);

	static_assert(std::same_as<decltype(*p2 / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p2 / as<D&>), D&>);
	delete p2;

	// D*, nullptr

	p2 = nullptr;

	assert(!(p2 / is<B*>));
	assert(p2 / as<B*> == nullptr);

	assert(!(p2 / is<D*>));
	assert(p2 / as<D*> == nullptr);
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
	assert(p / is<std::nullptr_t> && !p);

	p = make_unique<D>();
	assert(p / is<D*>);
	assert(p / as<D*> != nullptr);
	assert(p / is<std::nullptr_t> && !p);

	p = make_unique<D>();
	assert(*p / is<B&>);
	assert(*p / is<D&>);

	static_assert(std::same_as<decltype(*p / as<B&>), B&>);
	static_assert(std::same_as<decltype(*p / as<D&>), D&>);

	// base

	p = make_unique<B>();

	assert(p / is<B*>);
	assert(p / as<B*> != nullptr);
	assert(p / is<std::nullptr_t> && !p);

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
	assert(p2 / is<std::nullptr_t> && !p2);

	p2 = make_unique<D>();
	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);
	assert(p2 / is<std::nullptr_t> && !p2);

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

	// D*

	auto p2 = make_shared<D>();

	assert(p2 / is<B*>);
	assert(p2 / as<B*> != nullptr);

	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);

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

	// D*

	weak_ptr<D> p2 = pd;

	assert(p2 / is<B*>);
	assert(p2 / as<B*> != nullptr);

	assert(p2 / is<D*>);
	assert(p2 / as<D*> != nullptr);

	//assert(*p2 / is<B&>);
	//assert(*p2 / is<D&>);

	//static_assert(std::same_as<decltype(*p2 / as<B&>), B&>);
	//static_assert(std::same_as<decltype(*p2 / as<D&>), D&>);

	// D*, nullptr

	p2.reset();

	assert(!(p2 / is<B*>));
	assert(p2 / as<B*> == nullptr);

	assert(!(p2 / is<D*>));
	assert(p2 / as<D*> == nullptr);
}


int main() noexcept
{
	unit_test_native_ptr();
	unit_test_unique_ptr();
	unit_test_shared_ptr();
	unit_test_weak_ptr();
	unit_test_stl_types();
}
