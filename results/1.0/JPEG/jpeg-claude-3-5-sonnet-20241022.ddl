def Format =
{
    SOI := 0xFFD8;
    EOI := 0xFFD9;

    SOF0_Val := 0xFFC0;
    SOF1_Val := 0xFFC1;
    SOF2_Val := 0xFFC2;
    SOF3_Val := 0xFFC3;
    SOF5_Val := 0xFFC5;
    SOF6_Val := 0xFFC6;
    SOF7_Val := 0xFFC7;
    SOF9_Val := 0xFFC9;
    SOF10_Val := 0xFFCA;
    SOF11_Val := 0xFFCB;
    SOF13_Val := 0xFFCD;
    SOF14_Val := 0xFFCE;
    SOF15_Val := 0xFFCF;

    DHT := 0xFFC4;
    DQT := 0xFFDB;
    DRI := 0xFFDD;
    SOS := 0xFFDA;
    COM := 0xFFFE;

    APP0_Val := 0xFFE0;
    APP1_Val := 0xFFE1;
    APP2_Val := 0xFFE2;
    APP3_Val := 0xFFE3;
    APP4_Val := 0xFFE4;
    APP5_Val := 0xFFE5;
    APP6_Val := 0xFFE6;
    APP7_Val := 0xFFE7;
    APP8_Val := 0xFFE8;
    APP9_Val := 0xFFE9;
    APP10_Val := 0xFFEA;
    APP11_Val := 0xFFEB;
    APP12_Val := 0xFFEC;
    APP13_Val := 0xFFED;
    APP14_Val := 0xFFEE;
    APP15_Val := 0xFFEF;

    RST0 := 0xFFD0;
    RST1 := 0xFFD1;
    RST2 := 0xFFD2;
    RST3 := 0xFFD3;
    RST4 := 0xFFD4;
    RST5 := 0xFFD5;
    RST6 := 0xFFD6;
    RST7 := 0xFFD7;

    Component := {
        componentId : UINT8;
        samplingFactors : UINT8;
        quantizationTableId : UINT8;
    };

    FrameHeader := {
        length : UINT16;
        precision : UINT8;
        height : UINT16;
        width : UINT16;
        numComponents : UINT8;
        components : Component[numComponents];
    };

    HuffmanTable := {
        length : UINT16;
        tableInfo : UINT8;
        numSymbols : UINT8[16];
        symbols : UINT8[];
    };

    QuantizationTable := {
        length : UINT16;
        tableInfo : UINT8;
        values : UINT8[64] | UINT16[64];
    };

    ScanComponent := {
        componentId : UINT8;
        huffmanTableSelector : UINT8;
    };

    ScanHeader := {
        length : UINT16;
        numComponents : UINT8;
        components : ScanComponent[numComponents];
        spectralStart : UINT8;
        spectralEnd : UINT8;
        successiveApprox : UINT8;
    };

    JFIFHeader := {
        length : UINT16;
        identifier : UINT8[5];
        version : UINT16;
        units : UINT8;
        xDensity : UINT16;
        yDensity : UINT16;
        thumbWidth : UINT8;
        thumbHeight : UINT8;
        thumbData : UINT8[thumbWidth * thumbHeight * 3] if thumbWidth > 0 && thumbHeight > 0;
    };

    Segment := {
        marker : UINT16;
        data : union {
            case marker == SOF0_Val: FrameHeader;
            case marker == SOF1_Val: FrameHeader;
            case marker == SOF2_Val: FrameHeader;
            case marker == SOF3_Val: FrameHeader;
            case marker == SOF5_Val: FrameHeader;
            case marker == SOF6_Val: FrameHeader;
            case marker == SOF7_Val: FrameHeader;
            case marker == SOF9_Val: FrameHeader;
            case marker == SOF10_Val: FrameHeader;
            case marker == SOF11_Val: FrameHeader;
            case marker == SOF13_Val: FrameHeader;
            case marker == SOF14_Val: FrameHeader;
            case marker == SOF15_Val: FrameHeader;
            case marker == DHT: HuffmanTable;
            case marker == DQT: QuantizationTable;
            case marker == SOS: ScanHeader;
            case marker == APP0_Val: JFIFHeader;
            case marker >= APP1_Val && marker <= APP15_Val: {
                length : UINT16;
                data : UINT8[length-2];
            };
            case marker == COM: {
                length : UINT16;
                comment : UINT8[length-2];
            };
            case marker == DRI: {
                length : UINT16;
                interval : UINT16;
            };
        };
    };

    EntropyCodedData := {
        data : UINT8[] until EOI;
    };

    JPEG := {
        soi : SOI;
        segments : Segment[];
        imageData : EntropyCodedData;
        eoi : EOI;
    };
};