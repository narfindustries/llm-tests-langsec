// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "ntp_v4.h"

ntp_v4_t::ntp_v4_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, ntp_v4_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = this;
    m_reference_timestamp = 0;
    m_origin_timestamp = 0;
    m_receive_timestamp = 0;
    m_transmit_timestamp = 0;
    m_extension_field = 0;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void ntp_v4_t::_read() {
    m_flags = m__io->read_u1();
    m_stratum = m__io->read_u1();
    m_poll = m__io->read_s1();
    m_precision = m__io->read_s1();
    m_root_delay = m__io->read_s4be();
    m_root_dispersion = m__io->read_s4be();
    m_reference_id = m__io->read_u4be();
    m_reference_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    m_origin_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    m_receive_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    m_transmit_timestamp = new ntp_timestamp_t(m__io, this, m__root);
    n_extension_field = true;
    if ((_root()->flags() & 16) != 0) {
        n_extension_field = false;
        m_extension_field = new extension_t(m__io, this, m__root);
    }
    n_key_identifier = true;
    if ((_root()->flags() & 32) != 0) {
        n_key_identifier = false;
        m_key_identifier = m__io->read_u4be();
    }
    n_message_digest = true;
    if ((_root()->flags() & 32) != 0) {
        n_message_digest = false;
        m_message_digest = m__io->read_bytes(16);
    }
}

ntp_v4_t::~ntp_v4_t() {
    _clean_up();
}

void ntp_v4_t::_clean_up() {
    if (m_reference_timestamp) {
        delete m_reference_timestamp; m_reference_timestamp = 0;
    }
    if (m_origin_timestamp) {
        delete m_origin_timestamp; m_origin_timestamp = 0;
    }
    if (m_receive_timestamp) {
        delete m_receive_timestamp; m_receive_timestamp = 0;
    }
    if (m_transmit_timestamp) {
        delete m_transmit_timestamp; m_transmit_timestamp = 0;
    }
    if (!n_extension_field) {
        if (m_extension_field) {
            delete m_extension_field; m_extension_field = 0;
        }
    }
    if (!n_key_identifier) {
    }
    if (!n_message_digest) {
    }
}

ntp_v4_t::ntp_timestamp_t::ntp_timestamp_t(kaitai::kstream* p__io, ntp_v4_t* p__parent, ntp_v4_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void ntp_v4_t::ntp_timestamp_t::_read() {
    m_seconds = m__io->read_u4be();
    m_fraction = m__io->read_u4be();
}

ntp_v4_t::ntp_timestamp_t::~ntp_timestamp_t() {
    _clean_up();
}

void ntp_v4_t::ntp_timestamp_t::_clean_up() {
}

ntp_v4_t::extension_t::extension_t(kaitai::kstream* p__io, ntp_v4_t* p__parent, ntp_v4_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void ntp_v4_t::extension_t::_read() {
    m_field_type = m__io->read_u2be();
    m_length = m__io->read_u2be();
    m_value = m__io->read_bytes(length());
}

ntp_v4_t::extension_t::~extension_t() {
    _clean_up();
}

void ntp_v4_t::extension_t::_clean_up() {
}
