GZIP : STRUCT = {
    id1            : U8  = 0x1F;
    id2            : U8  = 0x8B;
    compression    : U8  = 0x08;
    flags          : Flags;
    mtime          : U32;
    xfl            : U8;
    os             : U8;
    extra          : ExtraField IF flags.fextra == 1;
    original_name  : STRING('NULL_TERMINATED') IF flags.fname == 1;
    comment        : STRING('NULL_TERMINATED') IF flags.fcomment == 1;
    header_crc16   : U16 IF flags.fhcrc == 1;
    compressed_data: BLOB;
    crc32          : U32;
    isize          : U32;
}

Flags : BITFIELD<U8> = {
    ftext          : 1;
    fhcrc          : 1;
    fextra         : 1;
    fname          : 1;
    fcomment       : 1;
    reserved       : 3;
}

ExtraField : STRUCT = {
    xlen           : U16;
    extra_data     : BLOB[xlen];
}