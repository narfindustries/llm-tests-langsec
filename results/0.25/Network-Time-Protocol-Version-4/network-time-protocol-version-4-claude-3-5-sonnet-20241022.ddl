def NTPv4 = {
    LI: uint2                            -- Leap Indicator
    VN: uint3                            -- Version Number
    Mode: uint3                          -- Mode
    Stratum: uint8                       -- Stratum
    Poll: int8                           -- Poll Interval
    Precision: int8                      -- Precision
    RootDelay: int32                     -- Root Delay (fixed-point 16.16)
    RootDispersion: uint32               -- Root Dispersion (fixed-point 16.16)
    ReferenceID: uint32                  -- Reference ID
    ReferenceTimestamp: uint64           -- Reference Timestamp
    OriginTimestamp: uint64              -- Origin Timestamp
    ReceiveTimestamp: uint64             -- Receive Timestamp
    TransmitTimestamp: uint64            -- Transmit Timestamp
    KeyID: uint32                        -- Key Identifier (optional)
    MessageDigest: bytes[16]             -- MD5 Message Digest (128 bits)
}

def LeapIndicator = uint2 where {
    0 -> NoWarning
    1 -> LastMinute61
    2 -> LastMinute59
    3 -> AlarmCondition
}

def Mode = uint3 where {
    0 -> Reserved
    1 -> SymmetricActive
    2 -> SymmetricPassive
    3 -> Client
    4 -> Server
    5 -> Broadcast
    6 -> ControlMessage
    7 -> PrivateUse
}

def Stratum = uint8 where {
    0 -> Unspecified
    1 -> PrimaryServer
    2..15 -> SecondaryServer
    16..255 -> Reserved
}

def NTPTimestamp = {
    Seconds: uint32                      -- Seconds since January 1, 1900
    Fraction: uint32                     -- Fraction of second
}