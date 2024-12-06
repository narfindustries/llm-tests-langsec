#ifndef NETWORK_TIME_PROTOCOL_VERSION_4_H_
#define NETWORK_TIME_PROTOCOL_VERSION_4_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

class network_time_protocol_version_4_t : public kaitai::kstruct {

public:

    network_time_protocol_version_4_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent = 0, network_time_protocol_version_4_t* p__root = 0);

private:
    void _read();
    void _clean_up();

public:
    ~network_time_protocol_version_4_t();

private:
    uint8_t m_leap_indicator;
    uint8_t m_version_number;
    uint8_t m_mode;
    uint8_t m_stratum;
    uint8_t m_poll;
    uint8_t m_precision;
    uint32_t m_root_delay;
    uint32_t m_root_dispersion;
    uint32_t m_reference_id;
    uint64_t m_reference_timestamp;
    uint64_t m_originate_timestamp;
    uint64_t m_receive_timestamp;
    uint64_t m_transmit_timestamp;
    network_time_protocol_version_4_t* m__root;
    kaitai::kstruct* m__parent;

public:

    /**
     * Leap indicator field
     */
    uint8_t leap_indicator() const { return m_leap_indicator; }

    /**
     * Version number field
     */
    uint8_t version_number() const { return m_version_number; }

    /**
     * Mode field
     */
    uint8_t mode() const { return m_mode; }

    /**
     * Stratum field
     */
    uint8_t stratum() const { return m_stratum; }

    /**
     * Poll interval
     */
    uint8_t poll() const { return m_poll; }

    /**
     * Precision of the clock
     */
    uint8_t precision() const { return m_precision; }

    /**
     * Total round-trip delay to the reference clock
     */
    uint32_t root_delay() const { return m_root_delay; }

    /**
     * Total dispersion to the reference clock
     */
    uint32_t root_dispersion() const { return m_root_dispersion; }

    /**
     * Reference clock identifier
     */
    uint32_t reference_id() const { return m_reference_id; }

    /**
     * Reference timestamp
     */
    uint64_t reference_timestamp() const { return m_reference_timestamp; }

    /**
     * Originate timestamp
     */
    uint64_t originate_timestamp() const { return m_originate_timestamp; }

    /**
     * Receive timestamp
     */
    uint64_t receive_timestamp() const { return m_receive_timestamp; }

    /**
     * Transmit timestamp
     */
    uint64_t transmit_timestamp() const { return m_transmit_timestamp; }
    network_time_protocol_version_4_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // NETWORK_TIME_PROTOCOL_VERSION_4_H_
