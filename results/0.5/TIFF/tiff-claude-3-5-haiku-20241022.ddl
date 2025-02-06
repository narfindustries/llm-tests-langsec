module Tiff {
    type ByteOrder = enum {
        LittleEndian = 0x4949,
        BigEndian = 0x4D4D
    }

    type MagicNumber = enum {
        StandardTiff = 0x002A
    }

    type FieldType = enum {
        Byte = 1,
        Ascii = 2,
        Short = 3,
        Long = 4,
        Rational = 5,
        SByte = 6,
        Undefined = 7,
        SShort = 8,
        SLong = 9,
        SRational = 10,
        Float = 11,
        Double = 12
    }

    type CompressionType = enum {
        NoCompression = 1,
        CcittModifiedHuffman = 2,
        CcittGroup3Fax = 3,
        CcittGroup4Fax = 4,
        Lzw = 5,
        Jpeg = 6,
        Packbits = 32773
    }

    type PhotometricInterpretation = enum {
        WhiteIsZero = 0,
        BlackIsZero = 1,
        Rgb = 2,
        PaletteColor = 3,
        TransparencyMask = 4,
        Cmyk = 5,
        YCbCr = 6
    }

    type Orientation = enum {
        TopLeft = 1,
        TopRight = 2,
        BottomRight = 3,
        BottomLeft = 4,
        LeftTop = 5,
        RightTop = 6,
        RightBottom = 7,
        LeftBottom = 8
    }

    type ResolutionUnit = enum {
        NoUnit = 1,
        Inches = 2,
        Centimeters = 3
    }

    type IfdEntry = struct {
        tag: u16,
        fieldType: FieldType,
        fieldCount: u32,
        fieldValueOrOffset: u32
    }

    type TiffHeader = struct {
        byteOrder: ByteOrder,
        magicNumber: MagicNumber,
        ifdOffset: u32
    }

    type IfdDirectory = struct {
        numEntries: u16,
        entries: IfdEntry[numEntries],
        nextIfdOffset: u32
    }

    type TagDefinitions = enum {
        ImageWidth = 0x0100,
        ImageLength = 0x0101,
        BitsPerSample = 0x0102,
        Compression = 0x0103,
        PhotometricInterpretation = 0x0106,
        StripOffsets = 0x0111,
        RowsPerStrip = 0x0116,
        StripByteCounts = 0x0117,
        XResolution = 0x011A,
        YResolution = 0x011B,
        PlanarConfiguration = 0x011C,
        ResolutionUnit = 0x0128,
        Software = 0x0131,
        DateTime = 0x0132,
        Artist = 0x013B,
        Predictor = 0x013D,
        ColorMap = 0x0140,
        SampleFormat = 0x0153,
        JpegTables = 0x015B
    }

    type TiffFile = struct {
        header: TiffHeader,
        imageFileDirectories: IfdDirectory[]
    }
}