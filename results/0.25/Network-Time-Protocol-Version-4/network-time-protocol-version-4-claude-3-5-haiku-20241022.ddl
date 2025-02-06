type NTPv4 = {
    leap_indicator: bit[2],
    version: bit[3],
    mode: bit[3],
    stratum: bit[8],
    poll_interval: int8,
    precision: int8,
    root_delay: int32,
    root_dispersion: uint32,
    reference_id: uint32,
    reference_timestamp: uint64,
    origin_timestamp: uint64,
    receive_timestamp: uint64,
    transmit_timestamp: uint64,
    extensions: list<ExtensionField>
}

type ExtensionField = {
    field_type: uint16,
    field_length: uint16,
    data: bit[]
}

type NTPTimestamp = {
    seconds: uint32,
    fraction: uint32
}