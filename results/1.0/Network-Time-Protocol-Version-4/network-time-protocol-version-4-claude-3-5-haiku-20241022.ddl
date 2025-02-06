type LeapIndicator = enum {
    NoWarning = 0,
    LastMinute61Seconds = 1,
    LastMinute59Seconds = 2,
    AlarmCondition = 3
}

type NTPMode = enum {
    Reserved = 0,
    SymmetricActive = 1,
    SymmetricPassive = 2,
    Client = 3,
    Server = 4,
    Broadcast = 5,
    ControlMessage = 6,
    PrivateUse = 7
}

type Stratum = uint8

type NTPHeader = bits {
    leap_indicator: LeapIndicator[2],
    version: uint[3],
    mode: NTPMode[3]
}

type NTPPacket = struct {
    header: NTPHeader,
    stratum: Stratum,
    poll_interval: int8,
    precision: int8,
    root_delay: float32,
    root_dispersion: float32,
    reference_id: uint32,
    reference_timestamp: float64,
    originate_timestamp: float64,
    receive_timestamp: float64,
    transmit_timestamp: float64,
    extensions: list(NTPExtension)?
}

type NTPExtension = struct {
    type: uint16,
    length: uint16,
    data: list(uint8)
}