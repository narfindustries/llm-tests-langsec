format TIFF {
    byteOrder: choice {
        littleEndian: U16le,
        bigEndian: U16be
    } = littleEndian;

    header {
        magic: U16 where value == 0x4949 || value == 0x4D4D;
        version: U16 where value == 42;
        firstIFDOffset: U32;
    }

    imageFileDirectory (IFD) {
        numEntries: U16;
        entries: Entry[numEntries];
        nextIFDOffset: U32;

        struct Entry {
            tag: U16;
            fieldType: U16;
            count: U32;
            valueOrOffset: U32;
        }
    }

    data {
        header: header;
        ifds: imageFileDirectory[];
    }

    constraints {
        header.magic == 0x4949 || header.magic == 0x4D4D;
        header.version == 42;
    }
}