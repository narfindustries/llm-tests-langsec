def Main = NTPv4Packet

def NTPv4Packet = {
    flags: uint8,
    stratum: uint8,
    poll: int8,
    precision: int8,
    root_delay: uint32,
    root_dispersion: uint32,
    reference_id: uint32,
    reference_timestamp: NTPTimestamp,
    origin_timestamp: NTPTimestamp,
    receive_timestamp: NTPTimestamp,
    transmit_timestamp: NTPTimestamp,
    auth: AuthenticationData
}

def NTPTimestamp = {
    seconds: uint32,
    fraction: uint32
}

def AuthenticationData = {
    key_identifier: uint32,
    message_digest: bytes
}

def GetLI(flags: uint8) = {
    (flags >> 6) & 0x3
}

def GetVN(flags: uint8) = {
    (flags >> 3) & 0x7
}

def GetMode(flags: uint8) = {
    flags & 0x7
}

def ValidPacket(p: NTPv4Packet) = {
    GetLI(p.flags) <= 3 &&
    GetVN(p.flags) == 4 &&
    GetMode(p.flags) <= 7 &&
    p.stratum <= 16 &&
    p.poll >= -6 &&
    p.poll <= 10
}