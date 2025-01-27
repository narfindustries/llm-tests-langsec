module NetworkTimeProtocolVersion4

type NTPPacket = struct {
    leap_indicator: bit<2>,
    version_number: bit<3>,
    mode: bit<3>,
    stratum: uint8,
    poll: int8,
    precision: int8,
    root_delay: int32,
    root_dispersion: uint32,
    reference_id: uint32,
    reference_timestamp: Timestamp,
    originate_timestamp: Timestamp,
    receive_timestamp: Timestamp,
    transmit_timestamp: Timestamp
}

type Timestamp = struct {
    seconds: uint32,
    fraction: uint32
}