#ifndef PNG_IMAGE_H_
#define PNG_IMAGE_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>
#include <vector>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

/**
 * PNG (Portable Network Graphics) is an image format that supports lossless compression.
 * This specification covers the basic structure of a PNG file.
 */

class png_image_t : public kaitai::kstruct {

public:
    class chunk_t;

    png_image_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent = 0, png_image_t* p__root = 0);

private:
    void _read();
    void _clean_up();

public:
    ~png_image_t();

    class chunk_t : public kaitai::kstruct {

    public:

        chunk_t(kaitai::kstream* p__io, png_image_t* p__parent = 0, png_image_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~chunk_t();

    private:
        bool f_is_ihdr;
        bool m_is_ihdr;

    public:
        bool is_ihdr();

    private:
        bool f_is_idat;
        bool m_is_idat;

    public:
        bool is_idat();

    private:
        bool f_is_iend;
        bool m_is_iend;

    public:
        bool is_iend();

    private:
        uint32_t m_length;
        std::string m_type;
        std::string m_data;
        uint32_t m_crc;
        png_image_t* m__root;
        png_image_t* m__parent;

    public:
        uint32_t length() const { return m_length; }
        std::string type() const { return m_type; }
        std::string data() const { return m_data; }
        uint32_t crc() const { return m_crc; }
        png_image_t* _root() const { return m__root; }
        png_image_t* _parent() const { return m__parent; }
    };

private:
    std::string m_signature;
    std::vector<chunk_t*>* m_chunks;
    png_image_t* m__root;
    kaitai::kstruct* m__parent;

public:
    std::string signature() const { return m_signature; }
    std::vector<chunk_t*>* chunks() const { return m_chunks; }
    png_image_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // PNG_IMAGE_H_
