enum Marker : uint16 {
    SOI = 0xFFD8
    APP0 = 0xFFE0
    APP1 = 0xFFE1
    COM = 0xFFFE
    DQT = 0xFFDB
    SOF0 = 0xFFC0
    DHT = 0xFFC4
    SOS = 0xFFDA
    EOI = 0xFFD9
    DRI = 0xFFDD
    RST0 = 0xFFD0
    RST1 = 0xFFD1
    RST2 = 0xFFD2
    RST3 = 0xFFD3
    RST4 = 0xFFD4
    RST5 = 0xFFD5
    RST6 = 0xFFD6
    RST7 = 0xFFD7
    SOF2 = 0xFFC2
}

struct JPEG {
    SOI soi
    Segment[] segments
    EOI eoi
}

struct SOI {
    Marker marker = Marker::SOI
}

struct EOI {
    Marker marker = Marker::EOI
}

struct Segment {
    Marker marker
    uint16 length
    uint8[length - 2] data
}

struct APP0Segment {
    uint8 identifier[5] // e.g., "JFIF\0"
    uint16 version
    uint8 units
    uint16 xDensity
    uint16 yDensity
    uint8 xThumbnail
    uint8 yThumbnail
    uint8[xThumbnail * yThumbnail * 3] thumbnailData
}

struct APP1Segment {
    uint8 identifier[6] // e.g., "Exif\0\0"
}

struct COMSegment {
    uint8[length - 2] comment
}

struct DQTSegment {
    struct QuantizationTable {
        uint8 info // Precision and identifier
        uint8[64] tableData
    }
    QuantizationTable[] tables
}

struct SOF0Segment {
    uint8 samplePrecision
    uint16 imageHeight
    uint16 imageWidth
    uint8 numComponents
    struct Component {
        uint8 componentId
        uint8 samplingFactors
        uint8 quantizationTableId
    }
    Component[numComponents] components
}

struct DHTSegment {
    struct HuffmanTable {
        uint8 info // Table class and identifier
        uint8[16] numCodes // Number of codes for each length
        uint8[] values // Values associated with codes
    }
    HuffmanTable[] tables
}

struct SOSSegment {
    uint8 numComponents
    struct ScanComponent {
        uint8 componentId
        uint8 huffmanTableIds // DC table id and AC table id
    }
    ScanComponent[numComponents] components
    uint8 startSpectralSelection
    uint8 endSpectralSelection
    uint8 approxBitPosition
    uint8[] compressedData // Until EOI or next marker
}

struct DRISegment {
    uint16 restartInterval
}