module TIFF {
    type bytes = u8[];

    enum ByteOrder : u16 {
        LittleEndian = 0x4949,
        BigEndian = 0x4D4D
    }

    enum Compression : u16 {
        None = 1,
        CCITT3 = 2,
        CCITT4 = 3,
        LZW = 4,
        JPEG = 5,
        OldJPEG = 6,
        PackBits = 32773
    }

    enum PhotometricInterpretation : u16 {
        WhiteIsZero = 0,
        BlackIsZero = 1,
        RGB = 2,
        PaletteColor = 3,
        TransparencyMask = 4,
        CMYK = 5,
        YCbCr = 6
    }

    enum Orientation : u16 {
        TopLeft = 1,
        TopRight = 2,
        BottomRight = 3,
        BottomLeft = 4,
        LeftTop = 5,
        RightTop = 6,
        RightBottom = 7,
        LeftBottom = 8
    }

    enum ResolutionUnit : u16 {
        None = 1,
        Inches = 2,
        Centimeters = 3
    }

    struct Rational {
        numerator: u32,
        denominator: u32
    }

    struct TIFFHeader {
        byteOrder: ByteOrder,
        magicNumber: u16 = 0x002A,
        ifdOffset: u32
    }

    struct IFDEntry {
        tag: u16,
        type: u16,
        count: u32,
        valueOrOffset: u32
    }

    struct ImageFileDirectory {
        numEntries: u16,
        entries: IFDEntry[numEntries],
        nextIFDOffset: u32
    }

    struct TIFFFile {
        header: TIFFHeader,
        primaryIFD: ImageFileDirectory,
        imageData: bytes
    }

    parser parse(input: TIFFFile) {
        let endian = match input.header.byteOrder {
            ByteOrder.LittleEndian => little,
            ByteOrder.BigEndian => big
        };
        return input;
    }
}