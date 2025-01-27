// Network Time Protocol (NTP) Version 4 Specification
module NetworkTimeProtocolV4

// NTP Packet Structure
type NtpPacket = {
    leap_indicator: 2 bits,
    version: 3 bits,
    mode: 3 bits,
    stratum: 8 bits,
    poll_interval: 8 bits,
    precision: 8 bits,
    root_delay: 32 bits,
    root_dispersion: 32 bits,
    reference_identifier: 32 bits,
    reference_timestamp: 64 bits,
    originate_timestamp: 64 bits,
    receive_timestamp: 64 bits,
    transmit_timestamp: 64 bits,
    optional_extensions: [ExtensionField]*
}

// Extension Field for Optional NTP Extensions
type ExtensionField = {
    field_type: 16 bits,
    field_length: 16 bits,
    field_data: [8 bits]*
}

// NTP Mode Definitions
const NTP_MODE_RESERVED = 0
const NTP_MODE_SYMMETRIC_ACTIVE = 1
const NTP_MODE_SYMMETRIC_PASSIVE = 2
const NTP_MODE_CLIENT = 3
const NTP_MODE_SERVER = 4
const NTP_MODE_BROADCAST = 5
const NTP_MODE_NTP_CONTROL_MESSAGE = 6
const NTP_MODE_PRIVATE = 7

// Leap Second Indicator Values
const LEAP_NO_WARNING = 0
const LEAP_LAST_MINUTE_61_SECONDS = 1
const LEAP_LAST_MINUTE_59_SECONDS = 2
const LEAP_ALARM_CLOCK_NOT_SYNCHRONIZED = 3

// Main parsing function
parse_ntp_packet = {
    packet: NtpPacket = {
        leap_indicator: read 2,
        version: read 3,
        mode: read 3,
        stratum: read 8,
        poll_interval: read 8,
        precision: read 8,
        root_delay: read 32,
        root_dispersion: read 32,
        reference_identifier: read 32,
        reference_timestamp: read 64,
        originate_timestamp: read 64,
        receive_timestamp: read 64,
        transmit_timestamp: read 64,
        optional_extensions: parse_optional_extensions()
    }
    return packet
}

// Optional extension fields parsing
parse_optional_extensions = {
    extensions: [ExtensionField] = []
    while remaining_bits() > 0 {
        extension: ExtensionField = {
            field_type: read 16,
            field_length: read 16,
            field_data: read_bytes(field_length)
        }
        extensions.append(extension)
    }
    return extensions
}