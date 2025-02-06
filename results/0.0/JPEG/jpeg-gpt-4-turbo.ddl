grammar JPEG;

import std.core;

struct JPEG_File {
    Segment segments[];
}

enum Marker : u16 {
    SOI  = 0xFFD8,
    EOI  = 0xFFD9,
    SOS  = 0xFFDA,
    DQT  = 0xFFDB,
    DNL  = 0xFFDC,
    DRI  = 0xFFDD,
    DHP  = 0xFFDE,
    EXP  = 0xFFDF,
    APP0 = 0xFFE0,
    APP15 = 0xFFEF,
    JPG0 = 0xFFF0,
    COM  = 0xFFFE,
    SOF0 = 0xFFC0,
    SOF2 = 0xFFC2,
    DHT  = 0xFFC4
}

struct Segment {
    Marker marker;
    switch(marker) {
        case Marker.SOI, Marker.EOI:
            void;
        case Marker.SOS:
            SOS_Segment data;
        case Marker.DQT:
            DQT_Segment data;
        case Marker.DHT:
            DHT_Segment data;
        case Marker.SOF0, Marker.SOF2:
            SOF_Segment data;
        case Marker.APP0..Marker.APP15:
            APP_Segment data;
        case Marker.COM:
            COM_Segment data;
        default:
            Unknown_Segment data;
    }
}

struct LengthPrefixedData {
    u16 length;
    u8 data[length - 2];
}

struct Unknown_Segment extends LengthPrefixedData {}

struct APP_Segment extends LengthPrefixedData {}

struct COM_Segment extends LengthPrefixedData {}

struct SOF_Segment {
    u16 length;
    u8 precision;
    u16 height;
    u16 width;
    u8 num_components;
    Component components[num_components];
}

struct Component {
    u8 id;
    u8 sampling_factors;
    u8 quant_table_id;
}

struct DQT_Segment {
    u16 length;
    QuantizationTable tables[];
}

struct QuantizationTable {
    u8 info;
    u8 data[64];
}

struct DHT_Segment {
    u16 length;
    HuffmanTable tables[];
}

struct HuffmanTable {
    u8 info;
    u8 lengths[16];
    u8 values[];
}

struct SOS_Segment {
    u16 length;
    u8 num_components;
    ScanComponent components[num_components];
    u8 start_spectral;
    u8 end_spectral;
    u8 approx_bits;
}

struct ScanComponent {
    u8 id;
    u8 huffman_table;
}

struct JPEG {
    JPEG_File file;
}