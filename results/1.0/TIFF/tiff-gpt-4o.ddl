endianness: "II" | "MM";

struct TIFF {
    UInt16 magic_number; // Should be 42
    UInt32 offset_of_first_IFD;

    IFD ifd;
}

struct IFD {
    UInt16 num_entries;
    Entry entries[num_entries];
    UInt32 next_ifd_offset; // 0 if no further IFDs
}

struct Entry {
    UInt16 tag;
    UInt16 type;
    UInt32 count;
    UInt32 value_offset;

    switch (tag) {
        case 256, 257: // ImageWidth, ImageLength
            UInt32 dimension;

        case 258: // BitsPerSample
            UInt16 bits_per_sample[count];

        case 259: // Compression
            UInt16 compression;

        case 262: // PhotometricInterpretation
            UInt16 photometric_interpretation;

        case 273: // StripOffsets
            UInt32 strip_offsets[count];

        case 277: // SamplesPerPixel
            UInt16 samples_per_pixel;

        case 278: // RowsPerStrip
            UInt32 rows_per_strip;

        case 279: // StripByteCounts
            UInt32 strip_byte_counts[count];

        case 282, 283: // XResolution, YResolution
            Rational resolution;

        case 284: // PlanarConfiguration
            UInt16 planar_configuration;

        case 296: // ResolutionUnit
            UInt16 resolution_unit;

        case 306: // DateTime
            Char[20] date_time; // "YYYY:MM:DD HH:MM:SS"

        case 315: // Artist
            Char artist[count];

        case 320: // ColorMap
            UInt16 color_map[count];

        case 322, 323: // TileWidth, TileLength
            UInt32 tile_dimension;

        case 324: // TileOffsets
            UInt32 tile_offsets[count];

        case 325: // TileByteCounts
            UInt32 tile_byte_counts[count];

        default:
            Byte unknown_value[type_size(type, count)];
    }
}

struct Rational {
    UInt32 numerator;
    UInt32 denominator;
}

function UInt32 type_size(UInt16 type, UInt32 count) {
    switch (type) {
        case 1: return 1 * count;  // BYTE
        case 2: return 1 * count;  // ASCII
        case 3: return 2 * count;  // SHORT
        case 4: return 4 * count;  // LONG
        case 5: return 8 * count;  // RATIONAL
        default: return 0;
    }
}