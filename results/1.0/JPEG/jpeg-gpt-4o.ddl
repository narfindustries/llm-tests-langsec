bitfield StartOfImage {
    byte 0xFF;
    byte 0xD8;
}

bitfield EndOfImage {
    byte 0xFF;
    byte 0xD9;
}

bitfield Marker {
    byte 0xFF;
    byte code;
}

let SegmentTypes = {
    SOF0: 0xC0,
    SOF2: 0xC2,
    DHT: 0xC4,
    DQT: 0xDB,
    DRI: 0xDD,
    SOS: 0xDA,
    APPn: 0xE0 .. 0xEF,
    COM: 0xFE
}

structure Segment {
    Marker marker;
    if marker.code not in [SegmentTypes.SOS, SegmentTypes.EOI] {
        uint16 length;
        uint8[length - 2] content;
    }
}

structure JpegFile {
    StartOfImage soi;
    while !eoi {
        Segment segment;
        if segment.marker.code == SegmentTypes.EOI {
            eoi = true;
        }
    }
    EndOfImage eoi;
}

JpegFile jpeg;