struct TIFFHeader {
    ByteOrder: byte_order;
    MagicNumber: uint16;
    IFDOffset: uint32;
}

struct IFDEntry {
    Tag: uint16;
    FieldType: uint16;
    Count: uint32;
    ValueOffset: uint32;
}

struct IFD {
    NumEntries: uint16;
    Entries: IFDEntry[NumEntries];
    NextIFDOffset: uint32;
}

struct TIFF {
    Header: TIFFHeader;
    IFDs: IFD[];
}

enum byte_order: uint16 {
    LittleEndian = 0x4949;
    BigEndian = 0x4D4D;
}

instance TIFF tiff {
    Header: TIFFHeader;
    IFDs: IFD[] {
        let offset = Header.IFDOffset;
        while (offset != 0) {
            seek(offset);
            let ifd = IFD;
            offset = ifd.NextIFDOffset;
            yield ifd;
        }
    }
}