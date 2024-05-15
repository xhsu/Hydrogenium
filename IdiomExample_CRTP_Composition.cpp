#include <print>

// Forward declr
struct CAK47;

struct CBaseWeapon
{
	virtual void ItemFrame() noexcept
	{
		std::println("{}", __FUNCTION__);
	}

	static void Create() noexcept
	{
		std::println("{}", __FUNCTION__);
	}
};

template <typename CFinal, typename Base>
struct Node1 : Base
{
	int32_t m_iClip{ 30 };
	bool m_bInReload{ false };

	void ItemFrame() noexcept override
	{
		Base::ItemFrame();
		std::println("{}", __FUNCTION__);
	}

	// This is the only way to access the static member for function sig. typedef in CFinal, for example.
	// Or just use requires following noexcept(E)
	template <typename CCompiledFinal = CFinal>
	static void Create() noexcept
	{
		Base::Create();
		std::println("{}: MaxClip == {}", __FUNCTION__, CCompiledFinal::MAX_CLIP);
	}
};

template <typename CFinal, typename Base>
struct Node2 : Base
{
	float m_flWalkSpeed{ 221.f };

	void ItemFrame() noexcept override
	{
		Base::ItemFrame();
		std::println("{}", __FUNCTION__);
	}

	static void Create() noexcept
	{
		Base::Create();
		std::println("{}", __FUNCTION__);
	}
};

template <typename CFinal, typename Base>
struct Node3 : Base
{
	void ItemFrame() noexcept override;

	static void Create() noexcept
	{
		Base::Create();
		std::println("{}", __FUNCTION__);
	}
};

template <typename CFinal, template <typename, typename> class TFirst, template <typename, typename> class... TRests>
struct Linker : TFirst<CFinal, Linker<CFinal, TRests...>>
{
};

template <typename CFinal, template <typename, typename> class TFirst>
struct Linker<CFinal, TFirst> : TFirst<CFinal, CBaseWeapon>
{
};

struct CAK47 : Linker<CAK47, Node1, Node2, Node3>
{
	static inline constexpr int32_t MAX_CLIP = 30;
};

template<typename CFinal, typename Base>
void Node3<CFinal, Base>::ItemFrame() noexcept
{
	// Accessing the data at final class doesn't requires a dynamic_cast.
	auto self = static_cast<CAK47*>(this);

	Base::ItemFrame();
	std::println("{}, m_iClip: {}, m_bInReload: {}, m_flWalkSpeed: {}", __FUNCTION__, self->m_iClip, self->m_bInReload, self->m_flWalkSpeed);
}

int main() noexcept
{
	CAK47::Create();

	CAK47 wpn{};
	wpn.ItemFrame();
}
