// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "network_time_protocol_version_4.h"

network_time_protocol_version_4_t::network_time_protocol_version_4_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, network_time_protocol_version_4_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = this;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void network_time_protocol_version_4_t::_read() {
    m_leap_indicator = m__io->read_u1();
    m_version_number = m__io->read_u1();
    m_mode = m__io->read_u1();
    m_stratum = m__io->read_u1();
    m_poll = m__io->read_u1();
    m_precision = m__io->read_u1();
    m_root_delay = m__io->read_u4be();
    m_root_dispersion = m__io->read_u4be();
    m_reference_id = m__io->read_u4be();
    m_reference_timestamp = m__io->read_u8be();
    m_originate_timestamp = m__io->read_u8be();
    m_receive_timestamp = m__io->read_u8be();
    m_transmit_timestamp = m__io->read_u8be();
}

network_time_protocol_version_4_t::~network_time_protocol_version_4_t() {
    _clean_up();
}

void network_time_protocol_version_4_t::_clean_up() {
}
