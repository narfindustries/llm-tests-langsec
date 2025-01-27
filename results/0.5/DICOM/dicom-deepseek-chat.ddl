struct DICOMFile {
    preamble: Preamble;
    prefix: Prefix;
    header: Header;
    data: Data;
}

struct Preamble {
    magic: U8[128];
}

struct Prefix {
    magic: U8[4];
}

struct Header {
    elements: Element[];
}

struct Element {
    tag: Tag;
    vr: VR;
    length: Length;
    value: Value;
}

struct Tag {
    group: U16;
    element: U16;
}

struct VR {
    value: U8[2];
}

struct Length {
    value: U16;
}

struct Value {
    data: U8[length.value];
}

struct Data {
    pixel_data: U8[];
}