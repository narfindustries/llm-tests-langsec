grammar JPEG;

import Basic;

type Marker = enum u16 {
    SOI = 0xFFD8,
    EOI = 0xFFD9,
    SOF0 = 0xFFC0,
    DHT = 0xFFC4,
    DQT = 0xFFDB,
    SOS = 0xFFDA,
    APP0 = 0xFFE0,
    COM = 0xFFFE,
    // other markers can be defined similarly
};

type QuantTable = struct {
    pq u8 : 4;
    tq u8 : 4;
    qk [u8] * 64;
};

type FrameComponentSpec = struct {
    componentId u8;
    samplingFactors u8;
    quantTableId u8;
};

type FrameHeader = struct {
    precision u8;
    height u16;
    width u16;
    numComponents u8;
    componentSpec [FrameComponentSpec] * numComponents;
};

type HuffmanTableSpec = struct {
    tc u8 : 4;
    th u8 : 4;
    numSymbols [u8] * 16;
    symbols [u8] *; // variable length, determined by numSymbols
};

type ScanComponentSpec = struct {
    cs u8;
    tdTa u8;
};

type ScanHeader = struct {
    numComponents u8;
    components [ScanComponentSpec] * numComponents;
    ss u8;
    se u8;
    ahAl u8;
};

type Segment = union {
    marker Marker;
    qt [QuantTable] if marker == Marker.DQT;
    frameHeader FrameHeader if marker == Marker.SOF0;
    huffmanTableSpec HuffmanTableSpec if marker == Marker.DHT;
    scanHeader ScanHeader if marker == Marker.SOS;
    appData [u8] * if marker == Marker.APP0;
    comment [u8] * if marker == Marker.COM;
    // add other cases for different markers
};

type JPEGFile = struct {
    segments [struct {
        marker Marker;
        data Segment;
    }] *;
};