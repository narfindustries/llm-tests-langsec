type GzipFile = struct {
    header: GzipHeader,
    fextra: optional<GzipFExtra> if header.flags.fextra,
    fname: optional<string> if header.flags.fname,
    fcomment: optional<string> if header.flags.fcomment,
    fhcrc: optional<u16> if header.flags.fhcrc,
    compressedData: bytes, // The actual compressed data (DEFLATE)
    footer: GzipFooter
};

type GzipHeader = struct {
    id1: u8 = 0x1F,
    id2: u8 = 0x8B,
    cm: u8 = 0x08,
    flags: GzipFlags,
    mtime: u32,
    xfl: u8,
    os: u8
};

type GzipFlags = bits<u8> {
    ftext: 1,
    fhcrc: 1,
    fextra: 1,
    fname: 1,
    fcomment: 1,
    reserved: 3
};

type GzipFExtra = struct {
    xlen: u16,
    xdata: bytes // Length specified by xlen
};

type GzipFooter = struct {
    crc32: u32,
    isize: u32
};