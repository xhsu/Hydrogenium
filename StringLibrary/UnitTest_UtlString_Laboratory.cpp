#include "Precompiled.hpp"
#include "UtlString.hpp"

using namespace Hydrogenium::String::Functors::Components;
using namespace Hydrogenium::String;
using namespace Hydrogenium::UnitTest;
using namespace Hydrogenium;

namespace Hydrogenium::Laboratory
{
	struct chr_fn_t : Linker<chr_fn_t, iter_default, cmp_default, callsig_view_char_size, dir_forward, ret_as_it_is>
	{
		static_assert(VerifyComponents<chr_fn_t>());

		using invk_sig::operator();

		using ctype_info = CType<char>;
		using char_type = ctype_info::char_type;
		using view_type = ctype_info::view_type;
		using iter_type = decltype(policy_dir::Begin(view_type{}));

		static constexpr auto Impl(ctype_info::view_type const& str, ctype_info::param_type ch, ptrdiff_t until) noexcept
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
}
