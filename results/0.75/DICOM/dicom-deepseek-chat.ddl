struct DICOMFile {
    preamble: Preamble;
    prefix: Prefix;
    header: Header;
    data: Data;
}

struct Preamble {
    magic: Magic;
    reserved: Reserved;
}

struct Magic {
    value: bytes[128];
}

struct Reserved {
    value: bytes[12];
}

struct Prefix {
    value: bytes[4];
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
    group: uint16;
    element: uint16;
}

struct VR {
    value: bytes[2];
}

struct Length {
    value: uint16;
}

struct Value {
    data: bytes[Length.value];
}

struct Data {
    elements: Element[];
}