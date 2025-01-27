module JPEG;

import UInt8;
import UInt16;
import UInt32;
import Array;

type JPEG = struct {
    marker: UInt16;
    segments: Array<Segment>;
};

type Segment = struct {
    marker: UInt16;
    length: UInt16;
    data: Array<UInt8>(length - 2);
};

let SOI_MARKER: UInt16 = 0xFFD8;
let EOI_MARKER: UInt16 = 0xFFD9;

let isValidMarker(marker: UInt16): Bool = {
    marker == SOI_MARKER || marker == EOI_MARKER || (marker >= 0xFFC0 && marker <= 0xFFFE)
};

let parseJPEG(data: Array<UInt8>): JPEG = {
    let pos = 0;
    let marker = UInt16(data[pos], data[pos + 1]);
    pos += 2;
    assert(marker == SOI_MARKER, "Invalid JPEG start of image marker");

    let segments = [];
    while (pos < data.length) {
        let segmentMarker = UInt16(data[pos], data[pos + 1]);
        pos += 2;
        assert(isValidMarker(segmentMarker), "Invalid JPEG segment marker");

        if (segmentMarker == EOI_MARKER) {
            break;
        }

        let segmentLength = UInt16(data[pos], data[pos + 1]);
        pos += 2;
        let segmentData = data.slice(pos, pos + segmentLength - 2);
        pos += segmentLength - 2;

        segments.push(Segment {
            marker: segmentMarker,
            length: segmentLength,
            data: segmentData
        });
    }

    return JPEG {
        marker: marker,
        segments: segments
    };
};