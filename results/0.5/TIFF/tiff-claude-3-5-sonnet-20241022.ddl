def ByteOrder = {
    II = LittleEndian
    MM = BigEndian
}

def TiffHeader = {
    order : ByteOrder
    version : U16 where version == 42
    first_ifd_offset : U32
}

def DataType = {
    BYTE = 1
    ASCII = 2
    SHORT = 3
    LONG = 4
    RATIONAL = 5
    SBYTE = 6
    UNDEFINED = 7
    SSHORT = 8
    SLONG = 9
    SRATIONAL = 10
    FLOAT = 11
    DOUBLE = 12
}

def SubfileType = {
    FullResolution = 0
    ReducedResolution = 1
    SinglePage = 2
    TransparencyMask = 3
}

def CompressionType = {
    None = 1
    CCITT_Group3_1D = 2
    CCITT_Group3_Fax = 3
    CCITT_Group4_Fax = 4
    LZW = 5
    JPEG_OLD = 6
    JPEG = 7
    PackBits = 32773
}

def PhotometricInterpretation = {
    WhiteIsZero = 0
    BlackIsZero = 1
    RGB = 2
    PaletteColor = 3
    TransparencyMask = 4
    CMYK = 5
    YCbCr = 6
    CIELab = 8
}

def Threshholding = {
    NoDithering = 1
    OrderedDither = 2
    RandomizedDither = 3
}

def FillOrder = {
    MSB_to_LSB = 1
    LSB_to_MSB = 2
}

def Orientation = {
    TopLeft = 1
    TopRight = 2
    BottomRight = 3
    BottomLeft = 4
    LeftTop = 5
    RightTop = 6
    RightBottom = 7
    LeftBottom = 8
}

def PlanarConfiguration = {
    Chunky = 1
    Planar = 2
}

def GrayResponseUnit = {
    Unit_0_1 = 1
    Unit_0_001 = 2
    Unit_0_0001 = 3
    Unit_0_00001 = 4
    Unit_0_000001 = 5
}

def ResolutionUnit = {
    NoUnit = 1
    Inch = 2
    Centimeter = 3
}

def ExtraSamples = {
    Unspecified = 0
    AssociatedAlpha = 1
    UnassociatedAlpha = 2
}

def TagType = {
    SubfileType = 254
    ImageWidth = 256
    ImageLength = 257
    BitsPerSample = 258
    Compression = 259
    PhotometricInterpretation = 262
    Threshholding = 263
    CellWidth = 264
    CellLength = 265
    FillOrder = 266
    DocumentName = 269
    ImageDescription = 270
    Make = 271
    Model = 272
    StripOffsets = 273
    Orientation = 274
    SamplesPerPixel = 277
    RowsPerStrip = 278
    StripByteCounts = 279
    MinSampleValue = 280
    MaxSampleValue = 281
    XResolution = 282
    YResolution = 283
    PlanarConfiguration = 284
    PageName = 285
    XPosition = 286
    YPosition = 287
    FreeOffsets = 288
    FreeByteCounts = 289
    GrayResponseUnit = 290
    GrayResponseCurve = 291
    ResolutionUnit = 296
    Software = 305
    DateTime = 306
    Artist = 315
    HostComputer = 316
    ColorMap = 320
    ExtraSamples = 338
    Copyright = 33432
}

def IFDEntry = {
    tag : TagType
    type : DataType
    count : U32
    value_offset : U32
}

def IFD = {
    num_entries : U16
    entries : IFDEntry[num_entries]
    next_ifd_offset : U32
}

def TIFF = {
    header : TiffHeader
    ifd : IFD
}

let tiff = TIFF