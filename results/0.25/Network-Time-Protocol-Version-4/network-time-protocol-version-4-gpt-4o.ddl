network_time_protocol_v4 : seq {
    li: u2,
    vn: u3,
    mode: u3,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: s32,
    root_dispersion: s32,
    reference_id: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    authenticator: authenticator ?,
}

authenticator : seq {
    key_identifier: u32,
    message_digest: u64,
}