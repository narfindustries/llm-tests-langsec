// Daedalus specification for JPEG file format

define JPEG {
    // JPEG files start with a Start Of Image (SOI) marker
    soi: u16be == 0xFFD8;

    // JPEG segments
    segments: Segment[];

    // End Of Image (EOI) marker
    eoi: u16be == 0xFFD9;
}

define Segment {
    // Each segment starts with a marker
    marker: u16be;

    // Length of the segment (excluding the marker itself)
    length: u16be;

    // Segment data
    data: bytes[length - 2];
}

// JPEG segments can have different markers, some of the common ones include:
// - 0xFFE0 to 0xFFEF: APP0 to APP15 (Application-specific segments)
// - 0xFFDB: DQT (Define Quantization Table)
// - 0xFFC0: SOF0 (Start Of Frame, Baseline DCT)
// - 0xFFC4: DHT (Define Huffman Table)
// - 0xFFDA: SOS (Start Of Scan)

enum SegmentType : u16be {
    SOF0 = 0xFFC0,
    DHT = 0xFFC4,
    DQT = 0xFFDB,
    SOS = 0xFFDA,
    EOI = 0xFFD9,
    APP0 = 0xFFE0,
    APP15 = 0xFFEF
}

define Segment {
    marker: SegmentType;
    length: u16be;
    data: bytes[length - 2];
}