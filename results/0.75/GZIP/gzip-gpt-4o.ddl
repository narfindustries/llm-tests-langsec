enum uint8 CompressionMethod {
    DEFLATE = 0x08
}

bitfield uint8 Flag {
    FTEXT = 0x01,
    FHCRC = 0x02,
    FEXTRA = 0x04,
    FNAME = 0x08,
    FCOMMENT = 0x10
}

enum uint8 OperatingSystem {
    FAT = 0x00,
    Amiga = 0x01,
    VMS = 0x02,
    Unix = 0x03,
    VM_CMS = 0x04,
    Atari_TOS = 0x05,
    HPFS = 0x06,
    Macintosh = 0x07,
    Z_System = 0x08,
    CPM = 0x09,
    TOPS_20 = 0x0A,
    NTFS = 0x0B,
    QDOS = 0x0C,
    Acorn_RISCOS = 0x0D,
    Unknown = 0xFF
}

struct Gzip {
    uint8 id1 = 0x1F;
    uint8 id2 = 0x8B;
    CompressionMethod cm;
    Flag flg;
    uint32 mtime;
    uint8 xfl;
    OperatingSystem os;

    if (flg & Flag.FEXTRA) {
        ExtraField extra;
    }

    if (flg & Flag.FNAME) {
        null_terminated_string fname;
    }

    if (flg & Flag.FCOMMENT) {
        null_terminated_string fcomment;
    }

    if (flg & Flag.FHCRC) {
        uint16 header_crc16;
    }

    CompressedData compressed_data;
    uint32 crc32;
    uint32 isize;
}

struct ExtraField {
    uint16 xlen;
    uint8[xlen] extra_data;
}

struct CompressedData {
    byte[] data;
}