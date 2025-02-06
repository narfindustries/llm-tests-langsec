type NtpTimestamp = {
    seconds: u32;
    fraction: u32;
}

type NtpPacket = {
    leap_indicator: u2;
    version: u3;
    mode: u3;
    stratum: u8;
    poll_interval: i8;
    precision: i8;
    root_delay: i32;
    root_dispersion: u32;
    reference_id: u32;
    reference_timestamp: NtpTimestamp;
    origin_timestamp: NtpTimestamp;
    receive_timestamp: NtpTimestamp;
    transmit_timestamp: NtpTimestamp;
    optional_extensions: [ExtensionField];
}

type ExtensionField = {
    field_type: u16;
    field_length: u16;
    data: [u8];
}