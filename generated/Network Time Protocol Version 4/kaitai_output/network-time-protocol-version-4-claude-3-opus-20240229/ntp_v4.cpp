// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "ntp_v4.h"

ntp_v4_t::ntp_v4_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, ntp_v4_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = this;
    m_reference_timestamp = 0;
    m_origin_timestamp = 0;
    m_receive_timestamp = 0;
    m_transmit_timestamp = 0;
    m_extension_fields = 0;

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
    m_extension_fields = new std::vector<extension_t*>();
    {
        int i = 0;
        extension_t* _;
        do {
            _ = new extension_t(m__io, this, m__root);
            m_extension_fields->push_back(_);
            i++;
        } while (!( ((_->next_field() == ntp_v4_t::EXTENSION_TYPE_EOP) || (_io()->is_eof())) ));
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
    if (m_extension_fields) {
        for (std::vector<extension_t*>::iterator it = m_extension_fields->begin(); it != m_extension_fields->end(); ++it) {
            delete *it;
        }
        delete m_extension_fields; m_extension_fields = 0;
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
    m__io__raw_value = 0;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void ntp_v4_t::extension_t::_read() {
    m_next_field = static_cast<ntp_v4_t::extension_type_t>(m__io->read_u2be());
    m_length = m__io->read_u2be();
    n_value = true;
    switch (next_field()) {
    case ntp_v4_t::EXTENSION_TYPE_MAC: {
        n_value = false;
        m__raw_value = m__io->read_bytes((length() - 4));
        m__io__raw_value = new kaitai::kstream(m__raw_value);
        m_value = new mac_data_t(m__io__raw_value, this, m__root);
        break;
    }
    default: {
        m__raw_value = m__io->read_bytes((length() - 4));
        break;
    }
    }
}

ntp_v4_t::extension_t::~extension_t() {
    _clean_up();
}

void ntp_v4_t::extension_t::_clean_up() {
    if (!n_value) {
        if (m__io__raw_value) {
            delete m__io__raw_value; m__io__raw_value = 0;
        }
        if (m_value) {
            delete m_value; m_value = 0;
        }
    }
}

ntp_v4_t::mac_data_t::mac_data_t(kaitai::kstream* p__io, ntp_v4_t::extension_t* p__parent, ntp_v4_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void ntp_v4_t::mac_data_t::_read() {
    m_key_id = m__io->read_u4be();
    m_mac_length = m__io->read_u2be();
    m_mac = m__io->read_bytes(mac_length());
}

ntp_v4_t::mac_data_t::~mac_data_t() {
    _clean_up();
}

void ntp_v4_t::mac_data_t::_clean_up() {
}
