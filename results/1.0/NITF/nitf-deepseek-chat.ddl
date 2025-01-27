struct NITFHeader {
    magic: b"NITF";
    version: uint8;
    header_length: uint32;
    file_length: uint64;
    num_segments: uint16;
    segments: Segment[num_segments];
}

struct Segment {
    segment_type: uint8;
    segment_length: uint32;
    data: byte[segment_length];
}

struct NITF {
    header: NITFHeader;
    segments: Segment[header.num_segments];
}