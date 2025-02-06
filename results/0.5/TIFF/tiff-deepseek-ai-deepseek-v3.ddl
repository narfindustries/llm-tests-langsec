TIFF = {
    header: Header,
    ifd: IFD
};

Header = {
    byte_order: ByteOrder,
    version: uint16,
    ifd_offset: uint32
};

ByteOrder = "II" | "MM";

IFD = {
    entry_count: uint16,
    entries: Entry[entry_count],
    next_ifd_offset: uint32
};

Entry = {
    tag: uint16,
    field_type: FieldType,
    value_count: uint32,
    value_offset: uint32
};

FieldType = uint16 where {
    _ == 1 => "BYTE",
    _ == 2 => "ASCII",
    _ == 3 => "SHORT",
    _ == 4 => "LONG",
    _ == 5 => "RATIONAL",
    _ == 6 => "SBYTE",
    _ == 7 => "UNDEFINED",
    _ == 8 => "SSHORT",
    _ == 9 => "SLONG",
    _ == 10 => "SRATIONAL",
    _ == 11 => "FLOAT",
    _ == 12 => "DOUBLE"
};

ImageWidth = uint16 | uint32;
ImageLength = uint16 | uint32;
BitsPerSample = uint16;
Compression = uint16;
PhotometricInterpretation = uint16;
Thresholding = uint16;
CellWidth = uint16;
CellLength = uint16;
FillOrder = uint16;
DocumentName = string;
ImageDescription = string;
Make = string;
Model = string;
StripOffsets = uint16 | uint32;
Orientation = uint16;
SamplesPerPixel = uint16;
RowsPerStrip = uint16 | uint32;
StripByteCounts = uint16 | uint32;
MinSampleValue = uint16;
MaxSampleValue = uint16;
XResolution = uint32;
YResolution = uint32;
PlanarConfiguration = uint16;
PageName = string;
XPosition = uint32;
YPosition = uint32;
FreeOffsets = uint32;
FreeByteCounts = uint32;
GrayResponseUnit = uint16;
GrayResponseCurve = uint16;
T4Options = uint32;
T6Options = uint32;
ResolutionUnit = uint16;
PageNumber = uint16;
TransferFunction = uint16;
Software = string;
DateTime = string;
Artist = string;
HostComputer = string;
Predictor = uint16;
WhitePoint = uint32;
PrimaryChromaticities = uint32;
ColorMap = uint16;
HalftoneHints = uint16;
TileWidth = uint16 | uint32;
TileLength = uint16 | uint32;
TileOffsets = uint32;
TileByteCounts = uint32;
InkSet = uint16;
InkNames = string;
NumberOfInks = uint16;
DotRange = uint8 | uint16;