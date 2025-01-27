struct JPEG {
    magic: magic = b"\xFF\xD8\xFF";
    segments: Segment[];
}

struct Segment {
    marker: uint8;
    length: uint16;
    data: bytes(length - 2);
}

instances JPEG {
    magic: b"\xFF\xD8\xFF";
}