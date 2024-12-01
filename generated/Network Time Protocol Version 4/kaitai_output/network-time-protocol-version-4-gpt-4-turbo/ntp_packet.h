#ifndef NTP_PACKET_H_
#define NTP_PACKET_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>
#include <vector>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

/**
 * Network Time Protocol (Version 4) to synchronize time over a network.
 */

class ntp_packet_t : public kaitai::kstruct {

public:
    class ntp_timestamp_t;
    class extension_field_t;

    enum leap_indicator_t {
        LEAP_INDICATOR_NO_WARNING = 0,
        LEAP_INDICATOR_LAST_MINUTE_61_SECONDS = 1,
        LEAP_INDICATOR_LAST_MINUTE_59_SECONDS = 2,
        LEAP_INDICATOR_ALARM_CONDITION = 3
    };

    enum mode_t {
        MODE_SYMMETRIC_ACTIVE = 1,
        MODE_SYMMETRIC_PASSIVE = 2,
        MODE_CLIENT = 3,
        MODE_SERVER = 4,
        MODE_BROADCAST = 5,
        MODE_NTP_CONTROL_MESSAGE = 6,
        MODE_PRIVATE_USE = 7
    };

    enum version_number_t {
        VERSION_NUMBER_VERSION_4 = 4
    };

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

        /**
         * Seconds since Jan 1, 1900.
         */
        uint32_t seconds() const { return m_seconds; }

        /**
         * Fraction of a second.
         */
        uint32_t fraction() const { return m_fraction; }
        ntp_packet_t* _root() const { return m__root; }
        ntp_packet_t* _parent() const { return m__parent; }
    };

    class extension_field_t : public kaitai::kstruct {

    public:

        extension_field_t(kaitai::kstream* p__io, ntp_packet_t* p__parent = 0, ntp_packet_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~extension_field_t();

    private:
        uint16_t m_field_type;
        uint16_t m_field_length;
        std::string m_field_data;
        ntp_packet_t* m__root;
        ntp_packet_t* m__parent;

    public:

        /**
         * Identifier of the type of extension field.
         */
        uint16_t field_type() const { return m_field_type; }

        /**
         * Length of the extension field.
         */
        uint16_t field_length() const { return m_field_length; }

        /**
         * Data of the extension field.
         */
        std::string field_data() const { return m_field_data; }
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
    uint8_t m_li_vn_mode;
    uint8_t m_stratum;
    uint8_t m_poll;
    uint8_t m_precision;
    uint32_t m_root_delay;
    uint32_t m_root_dispersion;
    uint32_t m_reference_id;
    ntp_timestamp_t* m_reference_timestamp;
    ntp_timestamp_t* m_originate_timestamp;
    ntp_timestamp_t* m_receive_timestamp;
    ntp_timestamp_t* m_transmit_timestamp;
    std::vector<extension_field_t*>* m_extension_fields;
    uint32_t m_key_id;
    uint64_t m_message_digest;
    ntp_packet_t* m__root;
    kaitai::kstruct* m__parent;

public:

    /**
     * Leap indicator, version and mode.
     */
    uint8_t li_vn_mode() const { return m_li_vn_mode; }

    /**
     * Stratum level of the local clock.
     */
    uint8_t stratum() const { return m_stratum; }

    /**
     * Maximum interval between successive messages.
     */
    uint8_t poll() const { return m_poll; }

    /**
     * Precision of the local clock.
     */
    uint8_t precision() const { return m_precision; }

    /**
     * Total round trip delay to the primary reference source.
     */
    uint32_t root_delay() const { return m_root_delay; }

    /**
     * Total dispersion to the primary reference source.
     */
    uint32_t root_dispersion() const { return m_root_dispersion; }

    /**
     * Identifier of the particular server or reference clock.
     */
    uint32_t reference_id() const { return m_reference_id; }

    /**
     * Time when the system clock was last set or corrected.
     */
    ntp_timestamp_t* reference_timestamp() const { return m_reference_timestamp; }

    /**
     * Time at the client when the request departed for the server.
     */
    ntp_timestamp_t* originate_timestamp() const { return m_originate_timestamp; }

    /**
     * Time at the server when the request arrived.
     */
    ntp_timestamp_t* receive_timestamp() const { return m_receive_timestamp; }

    /**
     * Time at the server when the response left for the client.
     */
    ntp_timestamp_t* transmit_timestamp() const { return m_transmit_timestamp; }

    /**
     * Optional extension fields. Exist only if length > 48 bytes.
     */
    std::vector<extension_field_t*>* extension_fields() const { return m_extension_fields; }

    /**
     * Key Identifier for authentication (optional).
     */
    uint32_t key_id() const { return m_key_id; }

    /**
     * Message Digest for authentication (optional).
     */
    uint64_t message_digest() const { return m_message_digest; }
    ntp_packet_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // NTP_PACKET_H_
