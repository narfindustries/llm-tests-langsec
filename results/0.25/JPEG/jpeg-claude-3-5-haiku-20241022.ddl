format JPEG {
    byteOrder = BigEndian;

    struct Header {
        magic: u8[2] where {
            this[0] == 0xFF && 
            this[1] == 0xD8
        };
    }

    struct SOFSegment {
        marker: u8[2] where {
            this[0] == 0xFF && 
            (this[1] == 0xC0 || this[1] == 0xC2)
        };
        length: u16;
        precision: u8;
        height: u16;
        width: u16;
        componentCount: u8;
    }

    struct DHTSegment {
        marker: u8[2] where {
            this[0] == 0xFF && 
            this[1] == 0xC4
        };
        length: u16;
        tableClass: u8;
        tableDestination: u8;
    }

    struct ScanSegment {
        marker: u8[2] where {
            this[0] == 0xFF && 
            this[1] == 0xDA
        };
        length: u16;
        componentCount: u8;
    }

    struct EOISegment {
        marker: u8[2] where {
            this[0] == 0xFF && 
            this[1] == 0xD9
        };
    }

    type File {
        header: Header;
        sofSegment: SOFSegment;
        dhtSegment: DHTSegment;
        scanSegment: ScanSegment;
        imageData: u8[];
        eoi: EOISegment;
    }
}