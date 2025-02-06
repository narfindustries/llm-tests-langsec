def NTP_Packet {
    LI : bits(2)
    VN : bits(3)
    Mode : bits(3)
    Stratum : u8
    Poll : i8
    Precision : i8
    RootDelay : i32
    RootDispersion : u32
    ReferenceID : bytes[4]
    ReferenceTimestamp : u64
    OriginTimestamp : u64
    ReceiveTimestamp : u64
    TransmitTimestamp : u64
    Authenticator ? {
        KeyIdentifier : u32
        MessageDigest : bytes
    }
}

def Main = NTP_Packet