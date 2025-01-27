module JPEG;

type JPEG = struct {
    segments: list<Segment>;
};

type Segment = struct {
    marker: u16be;
    length: u16be;
    data: bytes(length - 2);
};

let SOI = 0xFFD8;
let EOI = 0xFFD9;

let markers = {
    0xFFE0: "APP0",
    0xFFE1: "APP1",
    0xFFDB: "DQT",
    0xFFC0: "SOF0",
    0xFFC4: "DHT",
    0xFFDA: "SOS",
    0xFFDD: "DRI",
    0xFFFE: "COM"
};

let is_valid_marker(marker: u16be) = marker in markers;

let is_segment_start(marker: u16be) = marker != SOI && marker != EOI && is_valid_marker(marker);

let parse_segment = (data: bytes) -> Segment {
    let marker = u16be(data[0..2]);
    let length = u16be(data[2..4]);
    let segment_data = data[4..length + 2];
    return Segment { marker, length, segment_data };
};

let parse_segments = (data: bytes) -> list<Segment> {
    let segments = [];
    let offset = 0;
    while offset < len(data) {
        let marker = u16be(data[offset..offset+2]);
        if is_segment_start(marker) {
            let segment = parse_segment(data[offset..]);
            segments.append(segment);
            offset += segment.length + 2;
        } else {
            break;
        }
    }
    return segments;
};

let parse_jpeg = (data: bytes) -> JPEG {
    let segments = parse_segments(data[2..len(data)-2]);
    return JPEG { segments };
};

let jpeg_parser = parse_jpeg;