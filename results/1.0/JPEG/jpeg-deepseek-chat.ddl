struct JPEG {
    SOI: Marker = 0xFFD8;
    segments: Segment[] until $ == Marker(0xFFD9);
    EOI: Marker = 0xFFD9;
}

struct Segment {
    marker: Marker;
    length: be16 if marker != Marker(0xFFD8) && marker != Marker(0xFFD9);
    data: byte[length - 2] if marker != Marker(0xFFD8) && marker != Marker(0xFFD9);
}

enum Marker : be16 {
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
    APP10 = 0xFFEA;
    APP11 = 0xFFEB;
    APP12 = 0xFFEC;
    APP13 = 0xFFED;
    APP14 = 0xFFEE;
    APP15 = 0xFFEF;
    COM = 0xFFFE;
    DQT = 0xFFDB;
    DHT = 0xFFC4;
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
    SOS = 0xFFDA;
    EOI = 0xFFD9;
}