module JPEG;

import std.base;

alias Marker = U16;

enum JpegMarker : Marker {
    SOI  = 0xFFD8,
    EOI  = 0xFFD9,
    SOS  = 0xFFDA,
    DQT  = 0xFFDB,
    DNL  = 0xFFDC,
    DRI  = 0xFFDD,
    DHP  = 0xFFDE,
    EXP  = 0xFFDF,
    APP0 = 0xFFE0,
    APP1 = 0xFFE1,
    APP2 = 0xFFE2,
    APP3 = 0xFFE3,
    APP4 = 0xFFE4,
    APP5 = 0xFFE5,
    APP6 = 0xFFE6,
    APP7 = 0xFFE7,
    APP8 = 0xFFE8,
    APP9 = 0xFFE9,
    APPA = 0xFFEA,
    APPB = 0xFFEB,
    APPC = 0xFFEC,
    APPD = 0xFFED,
    APPE = 0xFFEE,
    APPF = 0xFFEF,
    JPG0 = 0xFFF0,
    JPG1 = 0xFFF1,
    JPG2 = 0xFFF2,
    JPG3 = 0xFFF3,
    JPG4 = 0xFFF4,
    JPG5 = 0xFFF5,
    JPG6 = 0xFFF6,
    SOF0 = 0xFFC0,
    SOF1 = 0xFFC1,
    SOF2 = 0xFFC2,
    SOF3 = 0xFFC3,
    SOF5 = 0xFFC5,
    SOF6 = 0xFFC6,
    SOF7 = 0xFFC7,
    SOF9 = 0xFFC9,
    SOFA = 0xFFCA,
    SOFB = 0xFFCB,
    SOFD = 0xFFCD,
    SOFE = 0xFFCE,
    SOFF = 0xFFCF,
    DHT  = 0xFFC4,
    DAC  = 0xFFCC,
    COM  = 0xFFFE,
    TEM  = 0xFF01
}

struct QuantizationTable {
    precision_and_id : U8;
    values : Array[U8, 64]; // Assuming 8x8 table
}

struct HuffmanTable {
    class_and_id : U8;
    lengths : Array[U8, 16];
    values : Array[U8]; // Variable length based on lengths
}

struct FrameComponentSpec {
    component_id : U8;
    sampling_factors : U8;
    quant_table_id : U8;
}

struct FrameHeader {
    length : U16;
    precision : U8;
    height : U16;
    width  : U16;
    num_components : U8;
    components : Array[FrameComponentSpec, dyn];
}

struct ScanComponentSpec {
    component_id : U8;
    huffman_table_id : U8;
}

struct ScanHeader {
    length : U16;
    num_components : U8;
    components : Array[ScanComponentSpec, dyn];
    spectral_start : U8;
    spectral_end : U8;
    approx_high_low : U8;
}

struct Segment {
    marker : JpegMarker;
    data : switch (marker) {
        case JpegMarker::DQT : Array[QuantizationTable, dyn];
        case JpegMarker::DHT : Array[HuffmanTable, dyn];
        case JpegMarker::SOF0, JpegMarker::SOF1, JpegMarker::SOF2, JpegMarker::SOF3,
             JpegMarker::SOF5, JpegMarker::SOF6, JpegMarker::SOF7, JpegMarker::SOF9,
             JpegMarker::SOFA, JpegMarker::SOFB, JpegMarker::SOFD, JpegMarker::SOFE,
             JpegMarker::SOFF : FrameHeader;
        case JpegMarker::SOS : ScanHeader;
        case JpegMarker::COM : Array[U8, dyn];
        default : Array[U8, dyn]; // Catch-all for unknown or unhandled segments
    };
}

struct JpegFile {
    soi : Marker;
    segments : Array[Segment, dyn];
    eoi : Marker;
}