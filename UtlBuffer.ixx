module;

// C
#include <cstdio>
#include <cstdlib>
#include <cstring>

export module UtlBuffer;

using byte = unsigned char;

export enum class seek
{
	set = SEEK_SET,
	cur = SEEK_CUR,
	end = SEEK_END,
};

#pragma warning(push)
#pragma warning(disable:4018)

export struct CBuffer
{
	CBuffer() noexcept
	{
		m_pbuf = nullptr;
		m_size = 0;
		m_seek = 0;
	}

	CBuffer(size_t size) noexcept
	{
		m_pbuf = (byte*)std::calloc(1, size);
		m_size = size;
		m_seek = 0;
	}

	~CBuffer() noexcept
	{
		if (m_pbuf)
		{
			std::free(m_pbuf);
			m_pbuf = nullptr;
		}

		m_size = 0;
		m_seek = 0;
	}

private:
	byte* GetSpace(size_t size) noexcept
	{
		if (!m_pbuf)
		{
			m_pbuf = (byte*)std::calloc(1, size);
			m_size = size;
		}

		if (m_seek + size > m_size)
		{
			m_size += size;
			m_pbuf = (byte*)realloc(m_pbuf, m_size);
		}

		byte* p = m_pbuf + m_seek;
		m_seek += size;

		return p;
	}

public:
	void Write(const void* data, size_t size) noexcept
	{
		byte* p = GetSpace(size);
		std::memcpy(p, data, size);
	}

	bool Read(void* pbuf, size_t size) noexcept
	{
		if (m_pbuf && m_seek < m_size)
		{
			size_t len = size;

			if (len > m_seek + m_size)
				len = m_size - m_seek;

			byte* p = m_pbuf + m_seek;
			m_seek += size;

			std::memcpy(pbuf, p, len);

			return true;
		}

		return false;
	}

	int getc(void) noexcept
	{
		if (m_pbuf && m_seek < m_size)
		{
			byte* p = m_pbuf + m_seek;
			m_seek++;

			return *p;
		}

		return EOF;
	}

	bool Seek(seek pos, int ofs) noexcept
	{
		switch (pos)
		{
		case seek::set:
			if (ofs >= 0 && ofs <= m_size)
			{
				m_seek = ofs;
				return true;
			}
			break;

		case seek::cur:
			if (m_seek + ofs >= 0 && m_seek + ofs <= m_size)
			{
				m_seek += ofs;
				return true;
			}
			break;

		case seek::end:
			if (m_size + ofs >= 0 && m_size + ofs <= m_size)
			{
				m_seek = m_size + ofs;
				return true;
			}
			break;
		}

		return false;
	}

	void* Get(void) const noexcept
	{
		return m_pbuf;
	}

	size_t Size(void) const noexcept
	{
		return m_size;
	}

	size_t Tell(void) const noexcept
	{
		return m_seek;
	}

private:
	byte* m_pbuf;

	size_t m_size;
	size_t m_seek;
};

#pragma warning(pop)
