struct DICOMFile {
    preamble: Preamble;
    prefix: Prefix;
    header: Header;
    data: Data;
}

struct Preamble {
    bytes: byte[128];
}

struct Prefix {
    magic: byte[4] = [0x44, 0x49, 0x43, 0x4D]; // "DICM"
}

struct Header {
    elements: Element[];
}

struct Element {
    group: uint16;
    element: uint16;
    vr: byte[2];
    length: uint16;
    value: byte[length];
}

struct Data {
    chunks: Chunk[];
}

struct Chunk {
    length: uint32;
    data: byte[length];
}