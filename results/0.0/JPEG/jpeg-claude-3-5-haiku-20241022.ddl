format JPEG {
    endian big;

    struct Header {
        magic: u8[2] where magic == [0xFF, 0xD8];
    }

    struct Segment {
        marker: u8[2];
        length: u16;
        data: u8[length - 2];
    }

    struct SOFSegment {
        precision: u8;
        height: u16;
        width: u16;
        components: u8;
    }

    struct Scan {
        components: u8;
        spectral_start: u8;
        spectral_end: u8;
        approx_high: u4;
        approx_low: u4;
    }

    type Image {
        header: Header;
        segments: Segment[];
        sof: SOFSegment;
        scan: Scan;
    }

    parse Image {
        header = Header {};
        segments = Segment[] where all segment.marker[0] == 0xFF;
        sof = SOFSegment {};
        scan = Scan {};
    }
}