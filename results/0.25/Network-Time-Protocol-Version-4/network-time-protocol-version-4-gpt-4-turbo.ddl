type NTPPacket = struct {
    li_version_mode UInt8;
    stratum         UInt8;
    poll            UInt8;
    precision       Int8;
    root_delay      UInt32;
    root_dispersion UInt32;
    reference_id    UInt32;
    ref_timestamp   UInt64;
    orig_timestamp  UInt64;
    recv_timestamp  UInt64;
    xmit_timestamp  UInt64;
    extensions      [NTPExtension] : until end;
    key_id          UInt32 ?;
    message_digest  UInt128 ?;
};

type NTPExtension = struct {
    field_type UInt16;
    field_len  UInt16;
    value      [UInt8] : size => field_len - 4;
};

type Int8 = int : width => 8;
type UInt8 = uint : width => 8;
type UInt16 = uint : width => 16;
type UInt32 = uint : width => 32;
type UInt64 = uint : width => 64;

type UInt128 = struct {
    high UInt64;
    low  UInt64;
};