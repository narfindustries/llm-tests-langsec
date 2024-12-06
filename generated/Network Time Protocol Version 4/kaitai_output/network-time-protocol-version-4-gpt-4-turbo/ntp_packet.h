#ifndef NTP_PACKET_H_
#define NTP_PACKET_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>
#include <vector>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

class ntp_packet_t : public kaitai::kstruct {

public:
    class ntp_timestamp_t;

    ntp_packet_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent = 0, ntp_packet_t* p__root = 0);

private:
    void _read();
    void _clean_up();

public:
    ~ntp_packet_t();

    class ntp_timestamp_t : public kaitai::kstruct {

    public:

        ntp_timestamp_t(kaitai::kstream* p__io, ntp_packet_t* p__parent = 0, ntp_packet_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~ntp_timestamp_t();

    private:
        uint32_t m_seconds;
        uint32_t m_fraction;
        ntp_packet_t* m__root;
        ntp_packet_t* m__parent;

    public:
        uint32_t seconds() const { return m_seconds; }
        uint32_t fraction() const { return m_fraction; }
        ntp_packet_t* _root() const { return m__root; }
        ntp_packet_t* _parent() const { return m__parent; }
    };

private:
    bool f_leap_indicator;
    int32_t m_leap_indicator;

public:
    int32_t leap_indicator();

private:
    bool f_version;
    int32_t m_version;

public:
    int32_t version();

private:
    bool f_mode;
    int32_t m_mode;

public:
    int32_t mode();

private:
    uint64_t m_li_vn_mode;
    uint8_t m_stratum;
    uint8_t m_poll;
    int8_t m_precision;
    int32_t m_root_delay;
    uint32_t m_root_dispersion;
    uint32_t m_reference_id;
    ntp_timestamp_t* m_reference_timestamp;
    ntp_timestamp_t* m_originate_timestamp;
    ntp_timestamp_t* m_receive_timestamp;
    ntp_timestamp_t* m_transmit_timestamp;
    std::vector<uint8_t>* m_extension_data;
    ntp_packet_t* m__root;
    kaitai::kstruct* m__parent;

public:
    uint64_t li_vn_mode() const { return m_li_vn_mode; }
    uint8_t stratum() const { return m_stratum; }
    uint8_t poll() const { return m_poll; }
    int8_t precision() const { return m_precision; }
    int32_t root_delay() const { return m_root_delay; }
    uint32_t root_dispersion() const { return m_root_dispersion; }
    uint32_t reference_id() const { return m_reference_id; }
    ntp_timestamp_t* reference_timestamp() const { return m_reference_timestamp; }
    ntp_timestamp_t* originate_timestamp() const { return m_originate_timestamp; }
    ntp_timestamp_t* receive_timestamp() const { return m_receive_timestamp; }
    ntp_timestamp_t* transmit_timestamp() const { return m_transmit_timestamp; }
    std::vector<uint8_t>* extension_data() const { return m_extension_data; }
    ntp_packet_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // NTP_PACKET_H_
