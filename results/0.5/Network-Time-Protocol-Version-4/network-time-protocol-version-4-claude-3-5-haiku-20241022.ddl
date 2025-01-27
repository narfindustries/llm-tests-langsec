// Network Time Protocol (NTP) Version 4 Specification
module Network-Time-Protocol-Version-4

// NTP Packet Structure
type NTPPacket = {
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

// Extension Field for optional NTP extensions
type ExtensionField = {
    field_type: 16 bits,
    field_length: 16 bits,
    field_data: [8 bits]*
}

// NTP Mode Definitions
const MODE_RESERVED = 0
const MODE_SYMMETRIC_ACTIVE = 1
const MODE_SYMMETRIC_PASSIVE = 2
const MODE_CLIENT = 3
const MODE_SERVER = 4
const MODE_BROADCAST = 5
const MODE_NTP_CONTROL_MESSAGE = 6
const MODE_PRIVATE = 7

// Leap Indicator Definitions
const LEAP_NO_WARNING = 0
const LEAP_LAST_MINUTE_61_SECONDS = 1
const LEAP_LAST_MINUTE_59_SECONDS = 2
const LEAP_ALARM = 3

// Parse NTP Packet
parse NTPPacket {
    leap_indicator = read 2 bits
    version = read 3 bits
    mode = read 3 bits
    stratum = read 8 bits
    poll_interval = read 8 bits
    precision = read 8 bits
    root_delay = read 32 bits
    root_dispersion = read 32 bits
    reference_identifier = read 32 bits
    reference_timestamp = read 64 bits
    originate_timestamp = read 64 bits
    receive_timestamp = read 64 bits
    transmit_timestamp = read 64 bits
    optional_extensions = repeat ExtensionField while remaining_data > 0
}

// Validate NTP Packet
validate NTPPacket {
    version == 4,
    mode in [MODE_SYMMETRIC_ACTIVE, MODE_SYMMETRIC_PASSIVE, 
             MODE_CLIENT, MODE_SERVER, MODE_BROADCAST, 
             MODE_NTP_CONTROL_MESSAGE, MODE_PRIVATE],
    leap_indicator in [LEAP_NO_WARNING, LEAP_LAST_MINUTE_61_SECONDS, 
                       LEAP_LAST_MINUTE_59_SECONDS, LEAP_ALARM]
}