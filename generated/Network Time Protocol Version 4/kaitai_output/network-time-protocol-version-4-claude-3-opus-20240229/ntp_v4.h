#ifndef NTP_V4_H_
#define NTP_V4_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

class ntp_v4_t : public kaitai::kstruct {

public:
    class ntp_timestamp_t;
    class extension_t;

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

        /**
         * Seconds since January 1, 1900
         */
        uint32_t seconds() const { return m_seconds; }

        /**
         * Fraction of second
         */
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
        uint16_t m_field_type;
        uint16_t m_length;
        std::string m_value;
        ntp_v4_t* m__root;
        ntp_v4_t* m__parent;

    public:

        /**
         * Field type
         */
        uint16_t field_type() const { return m_field_type; }

        /**
         * Length of the value field
         */
        uint16_t length() const { return m_length; }

        /**
         * Value
         */
        std::string value() const { return m_value; }
        ntp_v4_t* _root() const { return m__root; }
        ntp_v4_t* _parent() const { return m__parent; }
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
    extension_t* m_extension_field;
    bool n_extension_field;

public:
    bool _is_null_extension_field() { extension_field(); return n_extension_field; };

private:
    uint32_t m_key_identifier;
    bool n_key_identifier;

public:
    bool _is_null_key_identifier() { key_identifier(); return n_key_identifier; };

private:
    std::string m_message_digest;
    bool n_message_digest;

public:
    bool _is_null_message_digest() { message_digest(); return n_message_digest; };

private:
    ntp_v4_t* m__root;
    kaitai::kstruct* m__parent;

public:

    /**
     * Leap Indicator (2 bits) | Version Number (3 bits) | Mode (3 bits)
     */
    uint8_t flags() const { return m_flags; }

    /**
     * Stratum level of the clock
     */
    uint8_t stratum() const { return m_stratum; }

    /**
     * Maximum interval between successive messages (log2 seconds)
     */
    int8_t poll() const { return m_poll; }

    /**
     * Precision of the clock (log2 seconds)
     */
    int8_t precision() const { return m_precision; }

    /**
     * Total round trip delay time (seconds)
     */
    int32_t root_delay() const { return m_root_delay; }

    /**
     * Maximum error due to clock frequency tolerance (seconds)
     */
    int32_t root_dispersion() const { return m_root_dispersion; }

    /**
     * Reference clock identifier
     */
    uint32_t reference_id() const { return m_reference_id; }

    /**
     * Time when the clock was last set or corrected
     */
    ntp_timestamp_t* reference_timestamp() const { return m_reference_timestamp; }

    /**
     * Time at the client when the request departed for the server
     */
    ntp_timestamp_t* origin_timestamp() const { return m_origin_timestamp; }

    /**
     * Time at the server when the request arrived from the client
     */
    ntp_timestamp_t* receive_timestamp() const { return m_receive_timestamp; }

    /**
     * Time at the server when the response left for the client
     */
    ntp_timestamp_t* transmit_timestamp() const { return m_transmit_timestamp; }

    /**
     * Optional extension field
     */
    extension_t* extension_field() const { return m_extension_field; }

    /**
     * Optional key identifier
     */
    uint32_t key_identifier() const { return m_key_identifier; }

    /**
     * Optional message digest (MD5)
     */
    std::string message_digest() const { return m_message_digest; }
    ntp_v4_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // NTP_V4_H_
