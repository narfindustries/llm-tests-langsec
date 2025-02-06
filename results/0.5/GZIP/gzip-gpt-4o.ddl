enum CompressionMethod : uint8 {
    deflate = 8
}

enum OperatingSystem : uint8 {
    FAT = 0,
    Amiga = 1,
    VMS = 2,
    Unix = 3,
    VM_CMS = 4,
    Atari_TOS = 5,
    HPFS = 6,
    Macintosh = 7,
    Z_System = 8,
    CP_M = 9,
    TOPS_20 = 10,
    NTFS = 11,
    QDOS = 12,
    Acorn_RISCOS = 13,
    Unknown = 255
}

struct Flags : uint8 {
    bool FTEXT = (this & 0x01) != 0;
    bool FHCRC = (this & 0x02) != 0;
    bool FEXTRA = (this & 0x04) != 0;
    bool FNAME = (this & 0x08) != 0;
    bool FCOMMENT = (this & 0x10) != 0;
}

struct ExtraField {
    uint16 XLEN;
    byte[XLEN] subfields;
}

struct GZIP {
    uint8 ID1 { assert(this == 0x1F); }
    uint8 ID2 { assert(this == 0x8B); }
    CompressionMethod CM;
    Flags FLG;
    uint32 MTIME;
    uint8 XFL;
    OperatingSystem OS;
    
    if (FLG.FEXTRA) {
        ExtraField EXTRA;
    }
    
    if (FLG.FNAME) {
        string FNAME { terminator = 0x00; }
    }
    
    if (FLG.FCOMMENT) {
        string FCOMMENT { terminator = 0x00; }
    }
    
    if (FLG.FHCRC) {
        uint16 FHCRC;
    }
    
    // Compressed data
    byte[] compressed_data;
    
    // Footer
    uint32 CRC32;
    uint32 ISIZE;
}