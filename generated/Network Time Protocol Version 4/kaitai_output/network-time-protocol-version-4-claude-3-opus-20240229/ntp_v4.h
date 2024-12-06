#ifndef NTP_V4_H_
#define NTP_V4_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>
#include <vector>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

class ntp_v4_t : public kaitai::kstruct {

public:
    class ntp_timestamp_t;
    class extension_t;
    class mac_data_t;

    enum extension_type_t {
        EXTENSION_TYPE_EOP = 0,
        EXTENSION_TYPE_MAC = 1
    };

    ntp_v4_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent = 0, ntp_v4_t* p__root = 0);

private:
    void _read();
    void _clean_up();

public:
    ~ntp_v4_t();

    class ntp_timestamp_t : public kaitai::kstruct {

    public:

        ntp_timestamp_t(kaitai::kstream* p__io, ntp_v4_t* p__parent = 0, ntp_v4_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~ntp_timestamp_t();

    private:
        uint32_t m_seconds;
        uint32_t m_fraction;
        ntp_v4_t* m__root;
        ntp_v4_t* m__parent;

    public:
        uint32_t seconds() const { return m_seconds; }
        uint32_t fraction() const { return m_fraction; }
        ntp_v4_t* _root() const { return m__root; }
        ntp_v4_t* _parent() const { return m__parent; }
    };

    class extension_t : public kaitai::kstruct {

    public:

        extension_t(kaitai::kstream* p__io, ntp_v4_t* p__parent = 0, ntp_v4_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~extension_t();

    private:
        extension_type_t m_next_field;
        uint16_t m_length;
        mac_data_t* m_value;
        bool n_value;

    public:
        bool _is_null_value() { value(); return n_value; };

    private:
        ntp_v4_t* m__root;
        ntp_v4_t* m__parent;
        std::string m__raw_value;
        kaitai::kstream* m__io__raw_value;

    public:
        extension_type_t next_field() const { return m_next_field; }
        uint16_t length() const { return m_length; }
        mac_data_t* value() const { return m_value; }
        ntp_v4_t* _root() const { return m__root; }
        ntp_v4_t* _parent() const { return m__parent; }
        std::string _raw_value() const { return m__raw_value; }
        kaitai::kstream* _io__raw_value() const { return m__io__raw_value; }
    };

    class mac_data_t : public kaitai::kstruct {

    public:

        mac_data_t(kaitai::kstream* p__io, ntp_v4_t::extension_t* p__parent = 0, ntp_v4_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~mac_data_t();

    private:
        uint32_t m_key_id;
        uint16_t m_mac_length;
        std::string m_mac;
        ntp_v4_t* m__root;
        ntp_v4_t::extension_t* m__parent;

    public:
        uint32_t key_id() const { return m_key_id; }
        uint16_t mac_length() const { return m_mac_length; }
        std::string mac() const { return m_mac; }
        ntp_v4_t* _root() const { return m__root; }
        ntp_v4_t::extension_t* _parent() const { return m__parent; }
    };

private:
    uint8_t m_flags;
    uint8_t m_stratum;
    int8_t m_poll;
    int8_t m_precision;
    int32_t m_root_delay;
    int32_t m_root_dispersion;
    uint32_t m_reference_id;
    ntp_timestamp_t* m_reference_timestamp;
    ntp_timestamp_t* m_origin_timestamp;
    ntp_timestamp_t* m_receive_timestamp;
    ntp_timestamp_t* m_transmit_timestamp;
    std::vector<extension_t*>* m_extension_fields;
    ntp_v4_t* m__root;
    kaitai::kstruct* m__parent;

public:
    uint8_t flags() const { return m_flags; }
    uint8_t stratum() const { return m_stratum; }
    int8_t poll() const { return m_poll; }
    int8_t precision() const { return m_precision; }
    int32_t root_delay() const { return m_root_delay; }
    int32_t root_dispersion() const { return m_root_dispersion; }
    uint32_t reference_id() const { return m_reference_id; }
    ntp_timestamp_t* reference_timestamp() const { return m_reference_timestamp; }
    ntp_timestamp_t* origin_timestamp() const { return m_origin_timestamp; }
    ntp_timestamp_t* receive_timestamp() const { return m_receive_timestamp; }
    ntp_timestamp_t* transmit_timestamp() const { return m_transmit_timestamp; }
    std::vector<extension_t*>* extension_fields() const { return m_extension_fields; }
    ntp_v4_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // NTP_V4_H_
