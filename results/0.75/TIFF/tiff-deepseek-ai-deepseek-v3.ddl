tiff {
    header: Header;
    ifds: IFD[];
    imageData: ImageData;
}

Header {
    byteOrder: enum { LittleEndian = 0x4949; BigEndian = 0x4D4D; };
    magicNumber: const 0x002A;
    ifdOffset: UInt32;
}

IFD {
    numberOfEntries: UInt16;
    entries: Entry[numberOfEntries];
    nextIFDOffset: UInt32;
}

Entry {
    tag: UInt16;
    type: enum {
        Byte = 1;
        ASCII = 2;
        SHORT = 3;
        LONG = 4;
        RATIONAL = 5;
        SBYTE = 6;
        SSHORT = 7;
        SLONG = 8;
        SRATIONAL = 9;
        FLOAT = 10;
        DOUBLE = 11;
    };
    count: UInt32;
    valueOrOffset: switch (type) {
        case Byte: UInt8[count <= 4 ? count : 1];
        case ASCII: UInt8[count <= 4 ? count : 1];
        case SHORT: UInt16[count <= 2 ? count : 1];
        case LONG: UInt32[count == 1 ? 1 : 1];
        case RATIONAL: { numerator: UInt32; denominator: UInt32; }[count == 1 ? 1 : 1];
        case SBYTE: Int8[count <= 4 ? count : 1];
        case SSHORT: Int16[count <= 2 ? count : 1];
        case SLONG: Int32[count == 1 ? 1 : 1];
        case SRATIONAL: { numerator: Int32; denominator: Int32; }[count == 1 ? 1 : 1];
        case FLOAT: Float32[count == 1 ? 1 : 1];
        case DOUBLE: Float64[count == 1 ? 1 : 1];
    };
}

ImageData {
    strips: Strip[numberOfStrips];
}

Strip {
    offset: UInt32;
    byteCount: UInt32;
    data: UInt8[byteCount];
}