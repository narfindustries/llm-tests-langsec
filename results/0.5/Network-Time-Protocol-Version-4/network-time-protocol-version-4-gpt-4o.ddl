// Daedalus specification for Network Time Protocol Version 4 (NTPv4)

network_time_protocol {
    uint32_be seconds; // Seconds since 1900
    uint32_be fraction; // Fractional part of the timestamp

    uint8 leap_indicator: 2; // Leap second indicator
    uint8 version_number: 3; // Version number of the protocol
    uint8 mode: 3; // Mode of the message

    uint8 stratum; // Stratum level of the local clock
    int8 poll; // Maximum interval between successive messages
    int8 precision; // Precision of the local clock

    uint32_be root_delay; // Total round-trip delay to the primary reference source
    uint32_be root_dispersion; // Maximum error relative to the primary reference source

    uint32_be reference_id; // Reference identifier
    uint32_be reference_timestamp_seconds; // Reference timestamp (seconds part)
    uint32_be reference_timestamp_fraction; // Reference timestamp (fraction part)

    uint32_be originate_timestamp_seconds; // Originate timestamp (seconds part)
    uint32_be originate_timestamp_fraction; // Originate timestamp (fraction part)

    uint32_be receive_timestamp_seconds; // Receive timestamp (seconds part)
    uint32_be receive_timestamp_fraction; // Receive timestamp (fraction part)

    uint32_be transmit_timestamp_seconds; // Transmit timestamp (seconds part)
    uint32_be transmit_timestamp_fraction; // Transmit timestamp (fraction part)

    // Optional extension fields (if any)
    // Extensions are not defined in the base NTPv4 specification and are implementation-specific
    uint8* extensions[]; // Placeholder for optional extension fields

    // Authentication fields (if any)
    // Authentication is optional and depends on the implementation
    uint8* authentication[]; // Placeholder for optional authentication fields
}