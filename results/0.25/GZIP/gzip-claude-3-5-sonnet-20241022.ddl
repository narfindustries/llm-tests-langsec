def GzipFile = {
    ID1     : byte = 0x1f
    ID2     : byte = 0x8b
    CM      : byte = 8
    FLG     : bitfield(8) {
        FTEXT    : 1
        FHCRC    : 1
        FEXTRA   : 1
        FNAME    : 1
        FCOMMENT : 1
        reserved : 3
    }
    MTIME   : u32
    XFL     : byte
    OS      : byte

    if (FLG.FEXTRA) {
        XLEN    : u16
        EXTRA   : XLEN bytes
    }

    if (FLG.FNAME) {
        FNAME_DATA : string(term: 0x00)
    }

    if (FLG.FCOMMENT) {
        FCOMMENT_DATA : string(term: 0x00)
    }

    if (FLG.FHCRC) {
        HEADER_CRC : u16
    }

    COMPRESSED_DATA : bytes
    CRC32    : u32
    ISIZE    : u32
}

def Main = GzipFile