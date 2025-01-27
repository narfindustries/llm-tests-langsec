// Daedalus specification for Network Time Protocol Version 4 (NTPv4)
// This specification is designed to parse NTPv4 packets.

import "network-common.ddl"

struct NTPv4Packet {
    leap_indicator: uint2;
    version: uint3;
    mode: uint3;
    stratum: uint8;
    poll: uint8;
    precision: uint8;
    root_delay: uint32;
    root_dispersion: uint32;
    reference_id: uint32;
    reference_timestamp: uint64;
    origin_timestamp: uint64;
    receive_timestamp: uint64;
    transmit_timestamp: uint64;
    extension_fields: NTPExtensionFields;
}

struct NTPExtensionFields {
    count: uint8;
    fields: NTPExtensionField[count];
}

struct NTPExtensionField {
    field_type: uint16;
    field_length: uint16;
    field_value: uint8[field_length];
}

// Entry point for parsing NTPv4 packets
entry NTPv4Packet ntp_packet;