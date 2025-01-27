module JPEG {
    type JPEGFile = struct {
        segments: Segment*;
    }

    type Segment = struct {
        marker: Marker;
        data: switch (marker) {
            case SOI: SOI;
            case APP(_): APP;
            case DQT: DQT;
            case SOF0: SOF0;
            case DHT: DHT;
            case SOS: SOS;
            case EOI: EOI;
            default: UnknownSegment;
        }
    }

    type Marker = enum(u16) {
        SOI  = 0xFFD8,
        EOI  = 0xFFD9,
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
        APP10 = 0xFFEA,
        APP11 = 0xFFEB,
        APP12 = 0xFFEC,
        APP13 = 0xFFED,
        APP14 = 0xFFEE,
        APP15 = 0xFFEF,
        DQT  = 0xFFDB,
        SOF0 = 0xFFC0,
        DHT  = 0xFFC4,
        SOS  = 0xFFDA
    }

    type SOI = struct {}

    type EOI = struct {}

    type APP = struct {
        length: u16;
        data: u8[length - 2];
    }

    type DQT = struct {
        length: u16;
        data: u8[length - 2];
    }

    type SOF0 = struct {
        length: u16;
        data: u8[length - 2];
    }

    type DHT = struct {
        length: u16;
        data: u8[length - 2];
    }

    type SOS = struct {
        length: u16;
        data: u8[length - 2];
    }

    type UnknownSegment = struct {
        length: u16;
        data: u8[length - 2];
    }
}