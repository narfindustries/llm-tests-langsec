// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "png_image_claude_3_opus_20240229.h"
#include "kaitai/exceptions.h"

png_image_claude_3_opus_20240229_t::png_image_claude_3_opus_20240229_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, png_image_claude_3_opus_20240229_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = this;
    m_ihdr = 0;
    m_idat_chunks = 0;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void png_image_claude_3_opus_20240229_t::_read() {
    m_magic = m__io->read_bytes(8);
    if (!(magic() == std::string("\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", 8))) {
        throw kaitai::validation_not_equal_error<std::string>(std::string("\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", 8), magic(), _io(), std::string("/seq/0"));
    }
    m_ihdr_len = m__io->read_u4be();
    m_ihdr_type = m__io->read_bytes(4);
    if (!(ihdr_type() == std::string("\x49\x48\x44\x52", 4))) {
        throw kaitai::validation_not_equal_error<std::string>(std::string("\x49\x48\x44\x52", 4), ihdr_type(), _io(), std::string("/seq/2"));
    }
    m_ihdr = new ihdr_chunk_t(m__io, this, m__root);
    m_idat_chunks = new std::vector<chunk_t*>();
    {
        int i = 0;
        chunk_t* _;
        do {
            _ = new chunk_t(m__io, this, m__root);
            m_idat_chunks->push_back(_);
            i++;
        } while (!( ((_->type() == (std::string("IEND"))) || (_io()->is_eof())) ));
    }
}

png_image_claude_3_opus_20240229_t::~png_image_claude_3_opus_20240229_t() {
    _clean_up();
}

void png_image_claude_3_opus_20240229_t::_clean_up() {
    if (m_ihdr) {
        delete m_ihdr; m_ihdr = 0;
    }
    if (m_idat_chunks) {
        for (std::vector<chunk_t*>::iterator it = m_idat_chunks->begin(); it != m_idat_chunks->end(); ++it) {
            delete *it;
        }
        delete m_idat_chunks; m_idat_chunks = 0;
    }
}

png_image_claude_3_opus_20240229_t::ihdr_chunk_t::ihdr_chunk_t(kaitai::kstream* p__io, png_image_claude_3_opus_20240229_t* p__parent, png_image_claude_3_opus_20240229_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void png_image_claude_3_opus_20240229_t::ihdr_chunk_t::_read() {
    m_width = m__io->read_u4be();
    m_height = m__io->read_u4be();
    m_bit_depth = m__io->read_u1();
    m_color_type = m__io->read_u1();
    m_compression_method = m__io->read_u1();
    m_filter_method = m__io->read_u1();
    m_interlace_method = m__io->read_u1();
    m_crc = m__io->read_u4be();
}

png_image_claude_3_opus_20240229_t::ihdr_chunk_t::~ihdr_chunk_t() {
    _clean_up();
}

void png_image_claude_3_opus_20240229_t::ihdr_chunk_t::_clean_up() {
}

png_image_claude_3_opus_20240229_t::chunk_t::chunk_t(kaitai::kstream* p__io, png_image_claude_3_opus_20240229_t* p__parent, png_image_claude_3_opus_20240229_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void png_image_claude_3_opus_20240229_t::chunk_t::_read() {
    m_len = m__io->read_u4be();
    m_type = kaitai::kstream::bytes_to_str(m__io->read_bytes(4), std::string("UTF-8"));
    m_body = m__io->read_bytes(len());
    m_crc = m__io->read_u4be();
}

png_image_claude_3_opus_20240229_t::chunk_t::~chunk_t() {
    _clean_up();
}

void png_image_claude_3_opus_20240229_t::chunk_t::_clean_up() {
}
