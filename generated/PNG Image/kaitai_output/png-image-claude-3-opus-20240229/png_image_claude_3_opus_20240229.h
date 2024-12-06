#ifndef PNG_IMAGE_CLAUDE_3_OPUS_20240229_H_
#define PNG_IMAGE_CLAUDE_3_OPUS_20240229_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>
#include <vector>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

class png_image_claude_3_opus_20240229_t : public kaitai::kstruct {

public:
    class ihdr_chunk_t;
    class chunk_t;

    png_image_claude_3_opus_20240229_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent = 0, png_image_claude_3_opus_20240229_t* p__root = 0);

private:
    void _read();
    void _clean_up();

public:
    ~png_image_claude_3_opus_20240229_t();

    class ihdr_chunk_t : public kaitai::kstruct {

    public:

        ihdr_chunk_t(kaitai::kstream* p__io, png_image_claude_3_opus_20240229_t* p__parent = 0, png_image_claude_3_opus_20240229_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~ihdr_chunk_t();

    private:
        uint32_t m_width;
        uint32_t m_height;
        uint8_t m_bit_depth;
        uint8_t m_color_type;
        uint8_t m_compression_method;
        uint8_t m_filter_method;
        uint8_t m_interlace_method;
        uint32_t m_crc;
        png_image_claude_3_opus_20240229_t* m__root;
        png_image_claude_3_opus_20240229_t* m__parent;

    public:
        uint32_t width() const { return m_width; }
        uint32_t height() const { return m_height; }
        uint8_t bit_depth() const { return m_bit_depth; }
        uint8_t color_type() const { return m_color_type; }
        uint8_t compression_method() const { return m_compression_method; }
        uint8_t filter_method() const { return m_filter_method; }
        uint8_t interlace_method() const { return m_interlace_method; }
        uint32_t crc() const { return m_crc; }
        png_image_claude_3_opus_20240229_t* _root() const { return m__root; }
        png_image_claude_3_opus_20240229_t* _parent() const { return m__parent; }
    };

    class chunk_t : public kaitai::kstruct {

    public:

        chunk_t(kaitai::kstream* p__io, png_image_claude_3_opus_20240229_t* p__parent = 0, png_image_claude_3_opus_20240229_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~chunk_t();

    private:
        uint32_t m_len;
        std::string m_type;
        std::string m_body;
        uint32_t m_crc;
        png_image_claude_3_opus_20240229_t* m__root;
        png_image_claude_3_opus_20240229_t* m__parent;

    public:
        uint32_t len() const { return m_len; }
        std::string type() const { return m_type; }
        std::string body() const { return m_body; }
        uint32_t crc() const { return m_crc; }
        png_image_claude_3_opus_20240229_t* _root() const { return m__root; }
        png_image_claude_3_opus_20240229_t* _parent() const { return m__parent; }
    };

private:
    std::string m_magic;
    uint32_t m_ihdr_len;
    std::string m_ihdr_type;
    ihdr_chunk_t* m_ihdr;
    std::vector<chunk_t*>* m_idat_chunks;
    png_image_claude_3_opus_20240229_t* m__root;
    kaitai::kstruct* m__parent;

public:
    std::string magic() const { return m_magic; }
    uint32_t ihdr_len() const { return m_ihdr_len; }
    std::string ihdr_type() const { return m_ihdr_type; }
    ihdr_chunk_t* ihdr() const { return m_ihdr; }
    std::vector<chunk_t*>* idat_chunks() const { return m_idat_chunks; }
    png_image_claude_3_opus_20240229_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // PNG_IMAGE_CLAUDE_3_OPUS_20240229_H_
