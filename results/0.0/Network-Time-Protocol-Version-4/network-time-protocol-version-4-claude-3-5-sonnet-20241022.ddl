def Main = NTPv4Packet

def NTPv4Packet = {
    li : bits(2),
    vn : bits(3),
    mode : bits(3),
    stratum : uint8,
    poll : int8,
    precision : int8,
    root_delay : uint32,
    root_dispersion : uint32,
    reference_id : bits(32),
    reference_timestamp : uint64,
    origin_timestamp : uint64,
    receive_timestamp : uint64,
    transmit_timestamp : uint64,
    extensions : Many0 Extension
}

def Extension = {
    field_type : uint16,
    length : uint16,
    value : Take (length - 4) uint8
}

def Take n p = FSeq n p