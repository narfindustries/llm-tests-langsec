let IFH = {
    ByteOrder: uint16,
    Version: uint16,
    FirstIFDOffset: uint32
};

let TagType = {
    BYTE: 1,
    ASCII: 2,
    SHORT: 3,
    LONG: 4,
    RATIONAL: 5,
    SBYTE: 6,
    UNDEFINED: 7,
    SSHORT: 8,
    SLONG: 9,
    SRATIONAL: 10,
    FLOAT: 11,
    DOUBLE: 12
};

let Tag = {
    NewSubfileType: 254,
    SubfileType: 255,
    ImageWidth: 256,
    ImageLength: 257,
    BitsPerSample: 258,
    Compression: 259,
    PhotometricInterpretation: 262,
    Threshholding: 263,
    CellWidth: 264,
    CellLength: 265,
    FillOrder: 266,
    ImageDescription: 270,
    Make: 271,
    Model: 272,
    StripOffsets: 273,
    Orientation: 274,
    SamplesPerPixel: 277,
    RowsPerStrip: 278,
    StripByteCounts: 279,
    MinSampleValue: 280,
    MaxSampleValue: 281,
    XResolution: 282,
    YResolution: 283,
    PlanarConfiguration: 284,
    FreeOffsets: 288,
    FreeByteCounts: 289,
    GrayResponseUnit: 290,
    GrayResponseCurve: 291,
    ResolutionUnit: 296,
    Software: 305,
    DateTime: 306,
    Artist: 315,
    HostComputer: 316,
    ColorMap: 320,
    ExtraSamples: 338,
    Copyright: 33432
};

let Rational = {
    Numerator: uint32,
    Denominator: uint32
};

let SRational = {
    Numerator: int32,
    Denominator: int32
};

let IFDEntry = {
    TagId: uint16,
    DataType: uint16,
    DataCount: uint32,
    DataOffset: uint32
};

let IFD = {
    EntryCount: uint16,
    Entries: IFDEntry[EntryCount],
    NextIFDOffset: uint32
};

let TIFF = {
    Header: IFH,
    FirstIFD: IFD @ Header.FirstIFDOffset
};