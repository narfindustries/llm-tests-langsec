module TIFF {
    type Header = struct {
        byte_order: u16,
        magic_number: u16,
        first_ifd_offset: u32
    }

    type Rational = struct {
        numerator: u32,
        denominator: u32
    }

    type IFDEntry = struct {
        tag: u16,
        field_type: u16,
        count: u32,
        value_or_offset: u32
    }

    type IFD = struct {
        num_entries: u16,
        entries: IFDEntry[num_entries],
        next_ifd_offset: u32
    }

    type TIFFFile = struct {
        header: Header,
        ifds: list(IFD)
    }

    enum ByteOrder: u16 {
        LittleEndian = 0x4949,
        BigEndian = 0x4D4D
    }

    enum MagicNumber: u16 {
        Standard = 0x002A
    }

    enum TagType: u16 {
        BYTE = 1,
        ASCII = 2,
        SHORT = 3,
        LONG = 4,
        RATIONAL = 5
    }

    enum CompressionType: u16 {
        None = 1,
        CCITT_G3 = 2,
        CCITT_G4 = 3,
        LZW = 4,
        JPEG = 5,
        PackBits = 32773
    }

    enum PhotometricInterpretation: u16 {
        WhiteIsZero = 0,
        BlackIsZero = 1,
        RGB = 2,
        PaletteColor = 3,
        TransparencyMask = 4,
        CMYK = 5,
        YCbCr = 6
    }

    enum Tags: u16 {
        ImageWidth = 0x0100,
        ImageLength = 0x0101,
        BitsPerSample = 0x0102,
        Compression = 0x0103,
        PhotometricInterpretation = 0x0106,
        StripOffsets = 0x0111,
        RowsPerStrip = 0x0116,
        StripByteCounts = 0x0117
    }
}