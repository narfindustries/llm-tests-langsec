file TIFF {
    byteOrder: enum {
        BigEndian = 0x4D4D,
        LittleEndian = 0x4949
    }

    header: struct {
        byteOrder: byteOrder,
        magic: u16 = 42,
        ifdOffset: u32
    }

    ifd: struct {
        numEntries: u16,
        entries: array[numEntries] of entry,
        nextIfdOffset: u32 = 0
    }

    entry: struct {
        tag: u16,
        fieldType: u16,
        count: u32,
        valueOrOffset: u32
    }

    imageData: struct {
        stripOffsets: u32[],
        stripByteCounts: u32[]
    }

    constraints {
        header.byteOrder in [byteOrder.BigEndian, byteOrder.LittleEndian],
        header.magic == 42
    }

    parse {
        header,
        ifd,
        imageData
    }
}