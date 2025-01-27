module NetworkTimeProtocolVersion4

type NTPPacket = struct {
    leapIndicator: 2 bits
    versionNumber: 3 bits
    mode: 3 bits
    stratum: uint8
    poll: int8
    precision: int8
    rootDelay: int32
    rootDispersion: uint32
    referenceId: uint32
    referenceTimestamp: uint64
    originateTimestamp: uint64
    receiveTimestamp: uint64
    transmitTimestamp: uint64
}

root: NTPPacket