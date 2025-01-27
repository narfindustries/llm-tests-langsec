struct JPEG {
    magic: magic = b"\xFF\xD8";
    segments: Segment[] until $.magic == b"\xFF\xD9";
}

struct Segment {
    marker: U8;
    length: U16;
    data: U8[$.length - 2];
}

struct U8: uint8;
struct U16: uint16;