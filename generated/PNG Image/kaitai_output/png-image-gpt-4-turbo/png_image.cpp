// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "png_image.h"
#include "kaitai/exceptions.h"

png_image_t::png_image_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, png_image_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = this;
    m_chunks = 0;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void png_image_t::_read() {
    m_signature = m__io->read_bytes(8);
    if (!(signature() == std::string("\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", 8))) {
        throw kaitai::validation_not_equal_error<std::string>(std::string("\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", 8), signature(), _io(), std::string("/seq/0"));
    }
    m_chunks = new std::vector<chunk_t*>();
    {
        int i = 0;
        while (!m__io->is_eof()) {
            m_chunks->push_back(new chunk_t(m__io, this, m__root));
            i++;
        }
    }
}

png_image_t::~png_image_t() {
    _clean_up();
}

void png_image_t::_clean_up() {
    if (m_chunks) {
        for (std::vector<chunk_t*>::iterator it = m_chunks->begin(); it != m_chunks->end(); ++it) {
            delete *it;
        }
        delete m_chunks; m_chunks = 0;
    }
}

png_image_t::chunk_t::chunk_t(kaitai::kstream* p__io, png_image_t* p__parent, png_image_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;
    f_is_ihdr = false;
    f_is_idat = false;
    f_is_iend = false;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void png_image_t::chunk_t::_read() {
    m_length = m__io->read_u4le();
    m_type = kaitai::kstream::bytes_to_str(m__io->read_bytes(4), std::string("ASCII"));
    m_data = m__io->read_bytes(length());
    m_crc = m__io->read_u4le();
}

png_image_t::chunk_t::~chunk_t() {
    _clean_up();
}

void png_image_t::chunk_t::_clean_up() {
}

bool png_image_t::chunk_t::is_ihdr() {
    if (f_is_ihdr)
        return m_is_ihdr;
    m_is_ihdr = type() == (std::string("IHDR"));
    f_is_ihdr = true;
    return m_is_ihdr;
}

bool png_image_t::chunk_t::is_idat() {
    if (f_is_idat)
        return m_is_idat;
    m_is_idat = type() == (std::string("IDAT"));
    f_is_idat = true;
    return m_is_idat;
}

bool png_image_t::chunk_t::is_iend() {
    if (f_is_iend)
        return m_is_iend;
    m_is_iend = type() == (std::string("IEND"));
    f_is_iend = true;
    return m_is_iend;
}
