def ByteOrder = "II" | "MM"

def Header = struct {
    byte_order: ByteOrder,
    version: uint16,
    first_ifd_offset: uint32
}

def TagType = enum uint16 {
    BYTE = 1,
    ASCII = 2,
    SHORT = 3,
    LONG = 4,
    RATIONAL = 5,
    SBYTE = 6,
    UNDEFINED = 7,
    SSHORT = 8,
    SLONG = 9,
    SRATIONAL = 10,
    FLOAT = 11,
    DOUBLE = 12
}

def Tag = enum uint16 {
    NewSubfileType = 254,
    SubfileType = 255,
    ImageWidth = 256,
    ImageLength = 257,
    BitsPerSample = 258,
    Compression = 259,
    PhotometricInterpretation = 262,
    Threshholding = 263,
    CellWidth = 264,
    CellLength = 265,
    FillOrder = 266,
    DocumentName = 269,
    ImageDescription = 270,
    Make = 271,
    Model = 272,
    StripOffsets = 273,
    Orientation = 274,
    SamplesPerPixel = 277,
    RowsPerStrip = 278,
    StripByteCounts = 279,
    MinSampleValue = 280,
    MaxSampleValue = 281,
    XResolution = 282,
    YResolution = 283,
    PlanarConfiguration = 284,
    PageName = 285,
    XPosition = 286,
    YPosition = 287,
    FreeOffsets = 288,
    FreeByteCounts = 289,
    GrayResponseUnit = 290,
    GrayResponseCurve = 291,
    ResolutionUnit = 296,
    PageNumber = 297,
    TransferFunction = 301,
    Software = 305,
    DateTime = 306,
    Artist = 315,
    HostComputer = 316,
    Predictor = 317,
    WhitePoint = 318,
    PrimaryChromaticities = 319,
    ColorMap = 320,
    HalftoneHints = 321,
    TileWidth = 322,
    TileLength = 323,
    TileOffsets = 324,
    TileByteCounts = 325,
    InkSet = 332,
    InkNames = 333,
    NumberOfInks = 334,
    DotRange = 336,
    TargetPrinter = 337,
    ExtraSamples = 338,
    SampleFormat = 339,
    SMinSampleValue = 340,
    SMaxSampleValue = 341,
    TransferRange = 342,
    ClipPath = 343,
    XClipPathUnits = 344,
    YClipPathUnits = 345,
    Indexed = 346,
    JPEGTables = 347
}

def Compression = enum uint16 {
    None = 1,
    CCITT_Modified_Huffman_RLE = 2,
    CCITT_Group3_Fax = 3,
    CCITT_Group4_Fax = 4,
    LZW = 5,
    JPEG_Old = 6,
    JPEG = 7,
    PackBits = 32773
}

def PhotometricInterpretation = enum uint16 {
    WhiteIsZero = 0,
    BlackIsZero = 1,
    RGB = 2,
    Palette = 3,
    TransparencyMask = 4,
    CMYK = 5,
    YCbCr = 6,
    CIELab = 8
}

def Orientation = enum uint16 {
    TopLeft = 1,
    TopRight = 2,
    BottomRight = 3,
    BottomLeft = 4,
    LeftTop = 5,
    RightTop = 6,
    RightBottom = 7,
    LeftBottom = 8
}

def PlanarConfiguration = enum uint16 {
    Chunky = 1,
    Planar = 2
}

def ResolutionUnit = enum uint16 {
    None = 1,
    Inch = 2,
    Centimeter = 3
}

def Predictor = enum uint16 {
    None = 1,
    HorizontalDifferencing = 2
}

def InkSet = enum uint16 {
    CMYK = 1,
    NotCMYK = 2
}

def ExtraSamples = enum uint16 {
    Unspecified = 0,
    AssociatedAlpha = 1,
    UnassociatedAlpha = 2
}

def SampleFormat = enum uint16 {
    UnsignedInteger = 1,
    SignedInteger = 2,
    IEEEFloatingPoint = 3,
    Undefined = 4
}

def Rational = struct {
    numerator: uint32,
    denominator: uint32
}

def SRational = struct {
    numerator: int32,
    denominator: int32
}

def IFDEntry = struct {
    tag: Tag,
    type: TagType,
    count: uint32,
    value_offset: uint32
}

def IFD = struct {
    num_entries: uint16,
    entries: IFDEntry[num_entries],
    next_ifd_offset: uint32
}

def TIFF = struct {
    header: Header,
    ifd: IFD
}