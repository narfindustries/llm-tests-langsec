type ntp_packet {
    bits 8 li_vn_mode;
    uint8 stratum;
    uint8 poll;
    uint8 precision;
    uint32 root_delay;
    uint32 root_dispersion;
    uint32 reference_id;
    uint64 reference_timestamp;
    uint64 originate_timestamp;
    uint64 receive_timestamp;
    uint64 transmit_timestamp;
}

type ntp_fields {
    leap_indicator: bits 2;
    version_number: bits 3;
    mode: bits 3;
    stratum: uint8;
    poll: uint8;
    precision: uint8;
    root_delay: uint32;
    root_dispersion: uint32;
    reference_id: uint32;
    reference_timestamp: uint64;
    originate_timestamp: uint64;
    receive_timestamp: uint64;
    transmit_timestamp: uint64;
}

function parse_ntp_header(bits 8 li_vn_mode) -> ntp_fields {
    leap_indicator = li_vn_mode[6..7];
    version_number = li_vn_mode[3..5];
    mode = li_vn_mode[0..2];
    return ntp_fields(leap_indicator, version_number, mode, 0,0,0,0,0,0,0,0,0);
}
