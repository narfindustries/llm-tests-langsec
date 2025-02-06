enum SegmentMarker : uint16 {
    JPEG_APP0 = 0xFFE0,
    JPEG_APP1 = 0xFFE1,
    JPEG_APP2 = 0xFFE2,
    JPEG_APP3 = 0xFFE3,
    JPEG_APP4 = 0xFFE4,
    JPEG_APP5 = 0xFFE5,
    JPEG_APP6 = 0xFFE6,
    JPEG_APP7 = 0xFFE7,
    JPEG_APP8 = 0xFFE8,
    JPEG_APP9 = 0xFFE9,
    JPEG_APP10 = 0xFFEA,
    JPEG_APP11 = 0xFFEB,
    JPEG_APP12 = 0xFFEC,
    JPEG_APP13 = 0xFFED,
    JPEG_APP14 = 0xFFEE,
    JPEG_APP15 = 0xFFEF,
    JPEG_DQT = 0xFFDB,
    JPEG_SOF0 = 0xFFC0,
    JPEG_SOF1 = 0xFFC1,
    JPEG_SOF2 = 0xFFC2,
    JPEG_DHT = 0xFFC4,
    JPEG_SOS = 0xFFDA,
    JPEG_DRI = 0xFFDD,
    JPEG_COM = 0xFFFE,
    JPEG_EOI = 0xFFD9
}

Root {
    soi: SOI,
    segments: Segment[],
    eoi: EOI
}

SOI {
    marker: uint16 {assert(this == 0xFFD8)}
}

EOI {
    marker: uint16 {assert(this == 0xFFD9)}
}

Segment {
    marker: uint16,
    body: SegmentBody(marker)
}

SegmentBody(marker: uint16) = switch(marker) {
    SegmentMarker.JPEG_DQT => DQT,
    SegmentMarker.JPEG_DHT => DHT,
    SegmentMarker.JPEG_SOF0,
    SegmentMarker.JPEG_SOF1,
    SegmentMarker.JPEG_SOF2 => SOFx,
    SegmentMarker.JPEG_SOS => SOS,
    SegmentMarker.JPEG_DRI => DRI,
    SegmentMarker.JPEG_COM => COM,
    in [SegmentMarker.JPEG_APP0 .. SegmentMarker.JPEG_APP15] => AppData,
    _ => UnknownSegment
}

DQT {
    length: uint16 {assert(this >= 2)},
    tables: QuantizationTable[(length - 2) / 65]
}

QuantizationTable {
    pq_tq: uint8,
    qtable: uint8[64]
}

DHT {
    length: uint16 {assert(this >= 2)},
    tables: HuffmanTable[]
}

HuffmanTable {
    tc_th: uint8,
    lengths: uint8[16],
    values: uint8[]
}

SOFx {
    length: uint16 {assert(this >= 8)},
    precision: uint8,
    height: uint16,
    width: uint16,
    components: uint8,
    componentSpecs: ComponentSpec[components]
}

ComponentSpec {
    id: uint8,
    sampling: uint8,
    tq: uint8
}

SOS {
    length: uint16 {assert(this >= 6)},
    components: uint8,
    componentSpecs: ScanComponentSpec[components],
    spectralStart: uint8,
    spectralEnd: uint8,
    approx: uint8
}

ScanComponentSpec {
    cs: uint8,
    td_ta: uint8
}

DRI {
    length: uint16 {assert(this == 4)},
    restartInterval: uint16
}

COM {
    length: uint16 {assert(this >= 2)},
    comment: string[length - 2]
}

AppData {
    length: uint16 {assert(this >= 2)},
    data: bytes[length - 2]
}

UnknownSegment {
    length: uint16 {assert(this >= 2)},
    data: bytes[length - 2]
}