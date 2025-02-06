type NTPPacket = {
    leap_indicator: bits[2],
    version: bits[3],
    mode: bits[3],
    stratum: uint8,
    poll_interval: int8,
    precision: int8,
    root_delay: float32,
    root_dispersion: float32,
    reference_id: [uint8; 4],
    reference_timestamp: uint64,
    originate_timestamp: uint64,
    receive_timestamp: uint64,
    transmit_timestamp: uint64,
    authenticator: option<{
        key_id: uint32,
        message_digest: [uint8]
    }>
}