TIFF = struct {
    header: TIFFHeader,
    ifds: array of IFD
}

TIFFHeader = struct {
    endian: EndianType,
    magic_number: uint16, // Should be 42
    ifd_offset: uint32
}

EndianType = enum : uint16 {
    II = 0x4949, // Little Endian
    MM = 0x4D4D  // Big Endian
}

IFD = struct {
    num_entries: uint16,
    entries: array of DirectoryEntry(num_entries),
    next_ifd_offset: uint32
}

DirectoryEntry = struct {
    tag: uint16,
    type: DataType,
    count: uint32,
    value_offset: uint32
}

DataType = enum : uint16 {
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

const ImageWidth = 256
const ImageLength = 257
const BitsPerSample = 258
const Compression = 259
const PhotometricInterpretation = 262
const StripOffsets = 273
const SamplesPerPixel = 277
const RowsPerStrip = 278
const StripByteCounts = 279
const XResolution = 282
const YResolution = 283
const ResolutionUnit = 296
const ColorMap = 320
const TileWidth = 322
const TileLength = 323
const TileOffsets = 324
const TileByteCounts = 325
const JPEGInterchangeFormat = 513
const JPEGInterchangeFormatLength = 514

Rational = struct {
    numerator: uint32,
    denominator: uint32
}

DirectoryEntryValue = union(DirectoryEntry.tag) {
    case ImageWidth, ImageLength, TileWidth, TileLength:
        uint32 value;
    case BitsPerSample:
        array of uint16(DirectoryEntry.count) bits_per_sample;
    case Compression:
        uint16 compression_scheme;
    case PhotometricInterpretation:
        uint16 photometric;
    case StripOffsets, TileOffsets:
        array of uint32(DirectoryEntry.count) offsets;
    case SamplesPerPixel:
        uint16 samples_per_pixel;
    case RowsPerStrip:
        uint32 rows_per_strip;
    case StripByteCounts, TileByteCounts:
        array of uint32(DirectoryEntry.count) byte_counts;
    case XResolution, YResolution:
        Rational resolution;
    case ResolutionUnit:
        uint16 resolution_unit;
    case ColorMap:
        array of uint16(DirectoryEntry.count) color_map;
    case JPEGInterchangeFormat:
        uint32 jpeg_offset;
    case JPEGInterchangeFormatLength:
        uint32 jpeg_length;
}