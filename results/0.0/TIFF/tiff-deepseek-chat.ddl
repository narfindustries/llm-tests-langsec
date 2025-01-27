struct TIFFHeader {
    ByteOrder byteOrder;
    uint16 version;
    uint32 ifdOffset;
}

enum ByteOrder : uint16 {
    LittleEndian = 0x4949,
    BigEndian = 0x4D4D
}

struct IFDEntry {
    uint16 tag;
    uint16 type;
    uint32 count;
    uint32 valueOffset;
}

struct IFD {
    uint16 entryCount;
    IFDEntry[entryCount] entries;
    uint32 nextIFDOffset;
}

struct TIFF {
    TIFFHeader header;
    IFD[header.ifdOffset] ifds;
}

TIFF tiff;