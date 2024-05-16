#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::String::Functors::Components;
using namespace Hydrogenium::String;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;



namespace Hydrogenium::Laboratory
{
	template <
		typename char_t = char,
		template <typename, typename> class TComparator = Functors::Components::cmp_default,
		template <typename, typename> class TDirection = Functors::Components::dir_forward,
		template <typename, typename> class TIterator = Functors::Components::iter_default,

		template <typename, typename> class TCountRes = Functors::Components::ret_as_it_is,
		template <typename, typename> class TModifyRes = Functors::Components::ret_as_marshaled,
		template <typename, typename> class TQueryRes = Functors::Components::ret_as_it_is,
		template <typename, typename> class TSplitRes = Functors::Components::ret_as_it_is,
		template <typename, typename> class TTestRes = Functors::Components::ret_as_it_is
	>
	struct Utils final
	{
		struct detail final
		{
			static inline constexpr auto default_search_len = std::numeric_limits<ptrdiff_t>::max();

			// CSpn - v, v -> size_t; Use PBrk instead. This one will count distance from RangePolicy.Begin() instead of from absolute start. 'R' stands for 'relative'.
			template <typename functor, bool bSpnMode>
			static constexpr ptrdiff_t CSpnR(functor::view_type const& str, functor::view_type const& charset, ptrdiff_t count = default_search_len) noexcept
			{
				// The count is capping the str, not charset
				auto [b1, s1, e1] = functor::policy_iter::Get(str, typename functor::policy_dir{}, count);
				auto [b2, s2, e2] = functor::policy_iter::Get(charset, typename functor::policy_dir{}, default_search_len);
				ptrdiff_t counter = 0;

				for (; s1 < e1; functor::policy_iter::Arithmetic(s1, b1, e1, 1), ++counter)
				{
					bool found_in_src = false;
					auto const ch1 = functor::policy_iter::ValueOf(s1);

					for (s2 = b2; s2 < e2; functor::policy_iter::Arithmetic(s2, b2, e2, 1))
					{
						auto const ch2 = functor::policy_iter::ValueOf(s2);

						if (functor::policy_cmp::Eql(ch1, ch2))
						{
							found_in_src = true;
							break;
						}
					}

					if (found_in_src == !bSpnMode)
						break;
				}

				return counter;
			}

			// PBrk - v, v -> string_view; served as CSpn, Spn, SpnP as well.
			template <typename functor, bool bSpnPMode>
			static constexpr auto PBrk(functor::view_type const& str, functor::view_type const& charset, ptrdiff_t count = default_search_len) noexcept
			{
				// The count is capping the str, not charset
				auto [b1, s1, e1] = functor::policy_iter::Get(str, typename functor::policy_dir{}, count);
				auto [b2, s2, e2] = functor::policy_iter::Get(charset, typename functor::policy_dir{}, default_search_len);

				for (; s1 < e1; functor::policy_iter::Arithmetic(s1, b1, e1, 1))
				{
					bool found_in_src = false;
					auto const ch1 = functor::policy_iter::ValueOf(s1);

					for (s2 = b2; s2 < e2; functor::policy_iter::Arithmetic(s2, b2, e2, 1))
					{
						auto const ch2 = functor::policy_iter::ValueOf(s2);

						if (functor::policy_cmp::Eql(ch1, ch2))
						{
							found_in_src = true;
							break;
						}
					}

					if (found_in_src == !bSpnPMode)
						break;
				}

				// why can't I use goto in constexpr?!
			//LAB_POST_FINDING:;

				return std::make_tuple(std::move(b1), std::move(e1), std::move(s1));
			}
		};



#pragma region Basic functor declr

		// Why does these functor types so ugly?
		// https://en.cppreference.com/w/cpp/language/dependent_name

		// Declr in outter class is invisible with scoped presenting: like chr_fn_t::char_t.
#define INHERIT_TYPEDEFS					\
		using char_type = char_t;			\
		using ctype_info = CType<char_t>;	\
		using view_type = ctype_info::view_type;\
		using owner_type = ctype_info::owner_type;\
		using typename super::policy_cmp;	\
		using typename super::policy_dir;	\
		using typename super::policy_iter;	\
		using typename super::policy_ret;	\
		using iter_type = decltype(policy_dir::Begin(view_type{}));\
		using value_type = decltype(policy_iter::ValueOf(iter_type{}));\
		using super::operator()

#pragma endregion Basic functor declr
#define GENERATOR_TY std::experimental::generator


#pragma region Chr
		struct chr_fn_t : Linker<chr_fn_t, TIterator, TComparator, TDirection, TQueryRes>
		{
			using super = Linker<chr_fn_t, TIterator, TComparator, TDirection, TQueryRes>;
			INHERIT_TYPEDEFS;

			constexpr auto operator()(view_type const& str, ctype_info::param_type ch, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(policy_dir::Begin(str), policy_dir::End(str), policy_dir::Begin(str)))
			{
				auto [begin, it, end]
					= policy_iter::Get(str, policy_dir{}, until);

				for (; it < end; policy_iter::Arithmetic(it, begin, end, 1))
				{
					if (policy_cmp::Eql(policy_iter::ValueOf(it), ch))
						break;
				}

				return policy_ret::Transform(begin, end, it);
			}
		};
		static inline constexpr auto Chr = chr_fn_t{};
#pragma endregion Chr

#pragma region Cmp
		struct cmp_fn_t : Linker<cmp_fn_t, TIterator, TComparator, TDirection, TTestRes>
		{
			using super = Linker<cmp_fn_t, TIterator, TComparator, TDirection, TTestRes>;
			INHERIT_TYPEDEFS;

			constexpr auto operator()(view_type const& lhs, view_type const& rhs, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(0))
			{
				auto [b1, s1, e1] = policy_iter::Get(lhs, policy_dir{}, count);
				auto [b2, s2, e2] = policy_iter::Get(rhs, policy_dir{}, count);

				while (
					s1 < e1 && s2 < e2
					&& policy_cmp::Eql(policy_iter::ValueOf(s1), policy_iter::ValueOf(s2))
					)
				{
					policy_iter::Arithmetic(s1, b1, e1, 1);
					policy_iter::Arithmetic(s2, b2, e2, 1);
				}

				// Preventing deducing as something like 'int32_t'
				value_type const c1 = s1 == e1 ? '\0' : policy_iter::ValueOf(s1);
				value_type const c2 = s2 == e2 ? '\0' : policy_iter::ValueOf(s2);

				return policy_ret::Transform(policy_cmp::Cmp(c1, c2));
			}
		};
		static inline constexpr auto Cmp = cmp_fn_t{};
#pragma endregion Cmp

#pragma region Cnt
		struct cnt_fn_t : Linker<cnt_fn_t, TIterator, TComparator, TDirection, TCountRes>
		{
			using super = Linker<cnt_fn_t, TIterator, TComparator, TDirection, TCountRes>;
			INHERIT_TYPEDEFS;

			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(size_t{}))
			{
				auto [begin, it, end] = policy_iter::Get(str, policy_dir{}, count);

				size_t n = begin == end ? 0 : 1;	// #UPDATE_AT_CPP23 size type literal
				for (; policy_iter::Arithmetic(it, begin, end, 1) & StringPolicy::Iterating::APRES::MOVED; ++n) {}

				return policy_ret::Transform(n);
			}
		};
		static inline constexpr auto Cnt = cnt_fn_t{};
#pragma endregion Cnt

#pragma region Dup
		struct dup_fn_t : Linker<dup_fn_t, TIterator, TComparator, TDirection, TModifyRes>
		{
			using super = Linker<dup_fn_t, TIterator, TComparator, TDirection, TModifyRes>;
			INHERIT_TYPEDEFS;

			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(policy_dir::Begin(str), policy_dir::End(str)))
			{
				auto [_, it, logical_end] = policy_iter::Get(str, policy_dir{}, count);

				// the count param is for size in the native type. Not count of graphemes.
				return policy_ret::Transform(it, logical_end);
			}
		};
		static inline constexpr auto Dup = dup_fn_t{};
#pragma endregion Dup

#pragma region Lwr
		struct lwr_fn_t : Linker<lwr_fn_t, TIterator, TComparator, TDirection, TModifyRes>
		{
			using super = Linker<lwr_fn_t, TIterator, TComparator, TDirection, TModifyRes>;
			INHERIT_TYPEDEFS;

			constexpr auto operator()(view_type const& str, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(policy_dir::Begin(str), policy_dir::End(str), &CType<value_type>::ToLower))
			{
				auto [_, it, end] = policy_iter::Get(str, policy_dir{}, count);

				return policy_ret::Transform(it, end, &CType<value_type>::ToLower);
			}

			// return type is void, in the case of in_place mode.
			constexpr void operator()(owner_type* pstr, ptrdiff_t count = detail::default_search_len) const noexcept
			{
				auto [_, it, end] = policy_iter::Get(*pstr, policy_dir{}, count);

				static_assert(
					typeid(policy_ret::Transform(it, end, &CType<value_type>::ToLower)) == typeid(typename ctype_info::owner_type),
					"Lwr() method must be used with marshaled returning types."
				);

				*pstr = policy_ret::Transform(it, end, &CType<value_type>::ToLower);
			}
		};
		static inline constexpr auto Lwr = lwr_fn_t{};
#pragma endregion Lwr

#pragma region PBrk
		struct pbrk_fn_t : Linker<pbrk_fn_t, TIterator, TComparator, TDirection, TQueryRes>
		{
			using super = Linker<pbrk_fn_t, TIterator, TComparator, TDirection, TQueryRes>;
			INHERIT_TYPEDEFS;

			constexpr auto operator()(view_type const& str, view_type const& charset, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(policy_dir::Begin(str), policy_dir::End(str), policy_dir::Begin(str)))
			{
				auto [begin, end, it] = detail::template PBrk<pbrk_fn_t, false>(str, charset, count);

				return policy_ret::Transform(begin, end, it);
			}
		};
		static inline constexpr auto PBrk = pbrk_fn_t{};
#pragma endregion PBrk

#pragma region SpnP
		struct spnp_fn_t : Linker<spnp_fn_t, TIterator, TComparator, TDirection, TQueryRes>
		{
			using super = Linker<spnp_fn_t, TIterator, TComparator, TDirection, TQueryRes>;
			INHERIT_TYPEDEFS;

			constexpr auto operator()(view_type const& str, view_type const& charset, ptrdiff_t count = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(policy_dir::Begin(str), policy_dir::End(str), policy_dir::Begin(str)))
			{
				auto [begin, end, it] = detail::template PBrk<spnp_fn_t, true>(str, charset, count);

				return policy_ret::Transform(begin, end, it);
			}
		};
		static inline constexpr auto SpnP = spnp_fn_t{};
#pragma endregion SpnP

#pragma region Str
		struct str_fn_t : Linker<str_fn_t, TIterator, TComparator, TDirection, TQueryRes>
		{
			using super = Linker<str_fn_t, TIterator, TComparator, TDirection, TQueryRes>;
			INHERIT_TYPEDEFS;

			using HelperUtl = Utils<
				char_type,
				TComparator,
				Functors::Components::dir_forward,
				TIterator,

				TCountRes,
				TModifyRes,
				TQueryRes,
				TSplitRes,
				Functors::Components::ret_as_it_is
			>;

			constexpr auto operator()(view_type const& str, view_type const& substr, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(policy_dir::Begin(str), policy_dir::End(str), policy_dir::Begin(str)))
			{
				// Searching direction is nothing to do with comparing direction!!
				// the substr is going to attempting to match with the forwarding order.
				// hence no range function put onto src.

				auto [b1, s1, e1] = policy_iter::Get(str, policy_dir{}, until);
				auto const iSubstrCnt = HelperUtl::Cnt(substr, detail::default_search_len);	// compare up to the length of substr.

				if constexpr (!policy_dir::is_reverse)
				{
					for (; s1 < e1; policy_iter::Arithmetic(s1, b1, e1, 1))
					{
						if (HelperUtl::Cmp({ s1, e1 }, substr, iSubstrCnt) == 0)
							break;
					}
				}
				else
				{
					for (; s1 < e1; policy_iter::Arithmetic(s1, b1, e1, 1))
					{
						auto const fwit1 = ToForwardIter(s1);
						auto const fwed1 = ToForwardIter(b1, false);	// in reverse_iter, rbegin is the actual end.

						if (HelperUtl::Cmp({ fwit1, fwed1 }, substr, iSubstrCnt) == 0)
							break;
					}
				}

				return policy_ret::Transform(b1, e1, s1);
			}
		};
		static inline constexpr auto Str = str_fn_t{};
#pragma endregion Str

#pragma region Tok
		struct tok_fn_t : Linker<tok_fn_t, TIterator, TComparator, TDirection, TSplitRes>
		{
			using super = Linker<tok_fn_t, TIterator, TComparator, TDirection, TSplitRes>;
			INHERIT_TYPEDEFS;

			__forceinline static constexpr auto Impl(iter_type const& begin, iter_type& it, iter_type const& end, view_type const& delim) noexcept
				-> decltype(BuildStringView(begin, it, end, false))
			{
				// I. Move pointer to the next non-delim position.
				auto const org_before_comp = detail::template CSpnR<tok_fn_t, true>(BuildStringView(it, end, end, false), delim);
				policy_iter::Arithmetic(it, begin, end, org_before_comp);

				if (it >= end)
					return BuildStringView(end, end, end, false);

				// II. Move pointer to next delim position. And now 'it' serves as end.
				auto const tokenBegin = it;
				auto const org_before_comp2 = detail::template CSpnR<tok_fn_t, false>(BuildStringView(it, end, end, false), delim);
				policy_iter::Arithmetic(it, begin, end, org_before_comp2);

				// buffer is the end in this section as it is going to be assigned as '\0' in original strtok().
				return BuildStringView(tokenBegin, it, end, false);
			}

			[[nodiscard]]
			auto operator()(std::optional<view_type> const& str, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
				-> decltype(policy_ret::Transform(view_type{}))
			{
				static thread_local std::tuple_element_t<0, decltype(policy_iter::Get(*str, policy_dir{}, until))> begin{};
				static thread_local std::tuple_element_t<1, decltype(policy_iter::Get(*str, policy_dir{}, until))> it{};
				static thread_local std::tuple_element_t<2, decltype(policy_iter::Get(*str, policy_dir{}, until))> end{};

				if (str.has_value())
					std::tie(begin, it, end) = policy_iter::Get(*str, policy_dir{}, until);	// in all other calling case, 'until' param will be ignored.

				return policy_ret::Transform(Impl(begin, it, end, delim));
			}

			[[nodiscard]]
			auto operator()(StringPolicy::Result::as_generator_t, view_type str, view_type delim, ptrdiff_t until = detail::default_search_len) const noexcept
				-> GENERATOR_TY<view_type>
			{
				auto [begin, it, end] = policy_iter::Get(str, policy_dir{}, until);

				for (auto view = Impl(begin, it, end, delim); !view.empty(); view = Impl(begin, it, end, delim))
				{
					co_yield view;
				}

				co_return;
			}

			[[nodiscard]]
			constexpr auto operator()(StringPolicy::Result::as_vector_t, view_type const& str, view_type const& delim, ptrdiff_t until = detail::default_search_len) const noexcept
			{
				auto [begin, it, end] = policy_iter::Get(str, policy_dir{}, until);
				std::vector<view_type> ret{};

				for (auto view = Impl(begin, it, end, delim); !view.empty(); view = Impl(begin, it, end, delim))
				{
					ret.emplace_back(std::move(view));
				}

				return policy_ret::Transform(ret);
			}
		};
		static inline constexpr auto Tok = tok_fn_t{};
#pragma endregion Tok

#undef INHERIT_TYPEDEFS
	};
}

using namespace Hydrogenium::Laboratory;

using Str2 = Hydrogenium::Laboratory::Utils<>;

void UnitTest_Runtime()
{
	struct fake_functor
	{
		using ctype_info = CType<char>;
		using view_type = ctype_info::view_type;
		using iter_type = view_type::iterator;	// It's nothing but a node, nothing inside depending on the template param.
		using value_type = view_type::value_type;
	};

	assert((*Str2::Chr("abc", 'b') == 'b'));
	assert((Str2::Cmp("abc", "abc") == 0));
	assert((Str2::Cnt("abc") == 3));
	assert((Str2::Dup("abc") == "abc"));
	assert((Str2::Lwr("ABC") == "abc"));

	std::string test{ "ABCDEFG" };
	Str2::Lwr(&test);
	assert(test == "abcdefg");

	assert(*Str2::PBrk(test, "dg") == 'd');
	assert(*Str2::SpnP(test, "abd") == 'c');
	assert(Str2::Str(test, "abd") == std::string_view{ test }.end());
	assert(Str2::Str(test, "abc") == std::string_view{ test }.begin());
	assert(std::ranges::equal(Str2::Tok(StringPolicy::Result::as_vector, "hello, world", "\t, "), std::vector{ "hello", "world" }));
	assert(std::ranges::equal(Str2::Tok(StringPolicy::Result::as_generator, "hello, world", "\t, "), std::vector{ "hello", "world" }));
}
