// Network Time Protocol (NTP) Version 4 Specification
module NetworkTimeProtocolV4

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

type ExtensionField = {
    field_type: 16 bits,
    field_length: 16 bits,
    field_data: [u8]*
}

parser parse_ntp_packet(input: [u8]) -> NTPPacket {
    let leap_indicator = input[0] >> 6 & 0b11;
    let version = (input[0] >> 3) & 0b111;
    let mode = input[0] & 0b111;
    let stratum = input[1];
    let poll_interval = input[2];
    let precision = input[3];
    let root_delay = u32_from_bytes(input[4..8]);
    let root_dispersion = u32_from_bytes(input[8..12]);
    let reference_identifier = u32_from_bytes(input[12..16]);
    let reference_timestamp = u64_from_bytes(input[16..24]);
    let originate_timestamp = u64_from_bytes(input[24..32]);
    let receive_timestamp = u64_from_bytes(input[32..40]);
    let transmit_timestamp = u64_from_bytes(input[40..48]);

    // Optional extension fields parsing
    let mut extensions = [];
    let mut current_pos = 48;
    while current_pos < input.length {
        let field_type = u16_from_bytes(input[current_pos..current_pos+2]);
        let field_length = u16_from_bytes(input[current_pos+2..current_pos+4]);
        let field_data = input[current_pos+4..current_pos+4+field_length];
        
        extensions.push({
            field_type: field_type,
            field_length: field_length,
            field_data: field_data
        });

        current_pos += 4 + field_length;
    }

    return {
        leap_indicator: leap_indicator,
        version: version,
        mode: mode,
        stratum: stratum,
        poll_interval: poll_interval,
        precision: precision,
        root_delay: root_delay,
        root_dispersion: root_dispersion,
        reference_identifier: reference_identifier,
        reference_timestamp: reference_timestamp,
        originate_timestamp: originate_timestamp,
        receive_timestamp: receive_timestamp,
        transmit_timestamp: transmit_timestamp,
        optional_extensions: extensions
    }
}

fn u32_from_bytes(bytes: [u8]) -> u32 {
    return (bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3]
}

fn u64_from_bytes(bytes: [u8]) -> u64 {
    return (u64(bytes[0]) << 56) | 
           (u64(bytes[1]) << 48) | 
           (u64(bytes[2]) << 40) | 
           (u64(bytes[3]) << 32) |
           (u64(bytes[4]) << 24) | 
           (u64(bytes[5]) << 16) | 
           (u64(bytes[6]) << 8)  | 
           u64(bytes[7])
}