root = STRUCT {
    header : Header,
    compressedData : BINARY,
    crc32 : u32,
    isize : u32
}

Header = STRUCT {
    id1 : u8 { assert(this == 0x1f); },
    id2 : u8 { assert(this == 0x8b); },
    compressionMethod : u8 { assert(this == 0x08); },
    flags : Flags,
    modificationTime : u32,
    extraFlags : u8,
    operatingSystem : u8,
    extraField : ExtraField IF (flags.extra == 1),
    originalFilename : CString IF (flags.name == 1),
    fileComment : CString IF (flags.comment == 1),
    headerCRC : u16 IF (flags.crc == 1)
}

Flags = BITFIELD {
    text : u8:1,
    crc : u8:1,
    extra : u8:1,
    name : u8:1,
    comment : u8:1,
    reserved : u8:3
}

ExtraField = STRUCT {
    subfieldID : u16,
    subfieldLength : u16,
    subfieldData : BINARY[subfieldLength]
}