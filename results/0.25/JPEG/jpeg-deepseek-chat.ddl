struct JPEG {
    SOI: Marker = 0xFFD8;
    segments: Segment[] until $ == EOI;
    EOI: Marker = 0xFFD9;
}

struct Segment {
    marker: Marker;
    length: u16 if marker != RST and marker != SOI and marker != EOI;
    data: u8[length - 2] if marker != RST and marker != SOI and marker != EOI;
}

enum Marker: u16 {
    SOI = 0xFFD8;
    APP0 = 0xFFE0;
    APP1 = 0xFFE1;
    APP2 = 0xFFE2;
    APP3 = 0xFFE3;
    APP4 = 0xFFE4;
    APP5 = 0xFFE5;
    APP6 = 0xFFE6;
    APP7 = 0xFFE7;
    APP8 = 0xFFE8;
    APP9 = 0xFFE9;
    APPA = 0xFFEA;
    APPB = 0xFFEB;
    APPC = 0xFFEC;
    APPD = 0xFFED;
    APPE = 0xFFEE;
    APPF = 0xFFEF;
    SOF0 = 0xFFC0;
    SOF1 = 0xFFC1;
    SOF2 = 0xFFC2;
    SOF3 = 0xFFC3;
    SOF5 = 0xFFC5;
    SOF6 = 0xFFC6;
    SOF7 = 0xFFC7;
    SOF9 = 0xFFC9;
    SOF10 = 0xFFCA;
    SOF11 = 0xFFCB;
    SOF13 = 0xFFCD;
    SOF14 = 0xFFCE;
    SOF15 = 0xFFCF;
    DHT = 0xFFC4;
    DQT = 0xFFDB;
    DRI = 0xFFDD;
    SOS = 0xFFDA;
    RST0 = 0xFFD0;
    RST1 = 0xFFD1;
    RST2 = 0xFFD2;
    RST3 = 0xFFD3;
    RST4 = 0xFFD4;
    RST5 = 0xFFD5;
    RST6 = 0xFFD6;
    RST7 = 0xFFD7;
    COM = 0xFFFE;
    EOI = 0xFFD9;
}