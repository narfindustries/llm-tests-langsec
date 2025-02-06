include "std.ddl"

@LittleEndian
typedef struct gzip_header {
    id1         : u8 where id1 == 0x1f;
    id2         : u8 where id2 == 0x8b;
    cm          : u8 where cm == 0x08;
    flags       : flags_type;
    mtime       : u32;
    xfl         : u8;
    os          : u8 where os <= 13 || os == 255;
}

typedef bitfield[8] flags_type {
    ftext    : 0;
    fhcrc    : 1;
    fextra   : 2;
    fname    : 3;
    fcomment : 4;
    reserved : 5..7;
}

typedef struct gzip_optional {
    extra_field : struct {
        xlen : u16;
        data : u8[xlen];
    } if flags.fextra;
    filename    : string(0) if flags.fname;
    comment     : string(0) if flags.fcomment;
    header_crc  : u16 if flags.fhcrc;
}

typedef struct gzip_footer {
    crc32    : u32;
    isize    : u32;
}

typedef struct gzip_file {
    header          : gzip_header;
    optional_fields : gzip_optional;
    compressed_data : u8[] while offset < $-8;
    footer          : gzip_footer;
}