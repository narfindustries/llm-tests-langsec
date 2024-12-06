// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "ntp_packet.h"

ntp_packet_t::ntp_packet_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, ntp_packet_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = this;
    m_reference_timestamp = 0;
    m_originate_timestamp = 0;
    m_receive_timestamp = 0;
    m_transmit_timestamp = 0;
    m_extension_data = 0;
    f_leap_indicator = false;
    f_version = false;
    f_mode = false;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void ntp_packet_t::_read() {
    m_li_vn_mode = m__io->read_bits_int_be(8);
    m__io->align_to_byte();
    m_stratum = m__io->read_u1();
    m_poll = m__io->read_u1();
    m_precision = m__io->read_s1();
    m_root_delay = m__io->read_s4be();
    m_root_dispersion = m__io->read_u4be();
    m_reference_id = m__io->read_u4be();
    m_reference_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    m_originate_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    m_receive_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    m_transmit_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    m_extension_data = new std::vector<uint8_t>();
    const int l_extension_data = (_io()->size() - _io()->pos());
    for (int i = 0; i < l_extension_data; i++) {
        m_extension_data->push_back(m__io->read_u1());
    }
}

ntp_packet_t::~ntp_packet_t() {
    _clean_up();
}

void ntp_packet_t::_clean_up() {
    if (m_reference_timestamp) {
        delete m_reference_timestamp; m_reference_timestamp = 0;
    }
    if (m_originate_timestamp) {
        delete m_originate_timestamp; m_originate_timestamp = 0;
    }
    if (m_receive_timestamp) {
        delete m_receive_timestamp; m_receive_timestamp = 0;
    }
    if (m_transmit_timestamp) {
        delete m_transmit_timestamp; m_transmit_timestamp = 0;
    }
    if (m_extension_data) {
        delete m_extension_data; m_extension_data = 0;
    }
}

ntp_packet_t::ntp_timestamp_t::ntp_timestamp_t(kaitai::kstream* p__io, ntp_packet_t* p__parent, ntp_packet_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void ntp_packet_t::ntp_timestamp_t::_read() {
    m_seconds = m__io->read_u4be();
    m_fraction = m__io->read_u4be();
}

ntp_packet_t::ntp_timestamp_t::~ntp_timestamp_t() {
    _clean_up();
}

void ntp_packet_t::ntp_timestamp_t::_clean_up() {
}

int32_t ntp_packet_t::leap_indicator() {
    if (f_leap_indicator)
        return m_leap_indicator;
    m_leap_indicator = (li_vn_mode() >> 6);
    f_leap_indicator = true;
    return m_leap_indicator;
}

int32_t ntp_packet_t::version() {
    if (f_version)
        return m_version;
    m_version = ((li_vn_mode() & 56) >> 3);
    f_version = true;
    return m_version;
}

int32_t ntp_packet_t::mode() {
    if (f_mode)
        return m_mode;
    m_mode = (li_vn_mode() & 7);
    f_mode = true;
    return m_mode;
}
