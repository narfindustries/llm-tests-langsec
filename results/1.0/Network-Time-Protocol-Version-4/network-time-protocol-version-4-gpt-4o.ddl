module NetworkTimeProtocolVersion4 where

-- The Network Time Protocol (NTP) message format according to RFC 5905

struct NTPMessage {
    leapIndicator: uint2;
    versionNumber: uint3;
    mode: uint3;
    stratum: uint8;
    poll: int8;
    precision: int8;
    rootDelay: fixed32;  -- 16 bits integer part, 16 bits fractional part
    rootDispersion: fixed32;  -- 16 bits integer part, 16 bits fractional part
    referenceId: uint32;
    referenceTimestamp: Timestamp;
    originateTimestamp: Timestamp;
    receiveTimestamp: Timestamp;
    transmitTimestamp: Timestamp;
}

struct Timestamp {
    seconds: uint32;
    fraction: uint32;
}
