struct TIFFHeader {
    ByteOrder byteOrder;
    uint16 version;
    uint32 firstIFDOffset;
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
    uint16 numEntries;
    IFDEntry entries[numEntries];
    uint32 nextIFDOffset;
}

struct TIFF {
    TIFFHeader header;
    IFD ifds[];
}

TIFF tiff;