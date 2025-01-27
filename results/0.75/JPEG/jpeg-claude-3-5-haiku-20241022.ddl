format JPEG {
    byteOrder: BigEndian;

    let SOI = 0xFFD8;  // Start of Image marker
    let EOI = 0xFFD9;  // End of Image marker

    struct Header {
        startOfImage: u16 = SOI;
    }

    struct Segment {
        marker: u16;
        length: u16;
        data: bytes(length - 2);
    }

    struct App0Marker {
        identifier: string(5);  // "JFIF\0"
        version: u16;
        units: u8;
        xDensity: u16;
        yDensity: u16;
    }

    struct Quantization {
        precision: u4;
        tableId: u4;
        table: bytes(64);
    }

    struct HuffmanTable {
        tableClass: u4;
        tableId: u4;
        lengths: u8[16];
        values: u8[];
    }

    struct Frame {
        precision: u8;
        height: u16;
        width: u16;
        componentCount: u8;
        components: Component[componentCount];
    }

    struct Component {
        id: u8;
        samplingFactors: u8;
        quantTableId: u8;
    }

    struct Scan {
        componentCount: u8;
        components: ScanComponent[componentCount];
        startSpectralSelection: u8;
        endSpectralSelection: u8;
        successiveApproximation: u8;
    }

    struct ScanComponent {
        componentId: u8;
        dcHuffmanTable: u4;
        acHuffmanTable: u4;
    }

    type Image {
        header: Header;
        segments: Segment[];
        endOfImage: u16 = EOI;
    }
}