let DICOM = {
    preamble: Array<u8, 128>,
    prefix: "DICM",
    elements: Element[]
}

let Element = {
    tag: Tag,
    vr: optional<VR>,
    length: Length,
    value: Value
}

let Tag = {
    group: u16be,
    element: u16be
}

let VR = enum {
    AE = 0x4145, AS = 0x4153, AT = 0x4154,
    CS = 0x4353, DA = 0x4441, DS = 0x4453,
    DT = 0x4454, FL = 0x464C, FD = 0x4644,
    IS = 0x4953, LO = 0x4C4F, LT = 0x4C54,
    OB = 0x4F42, OF = 0x4F46, OW = 0x4F57,
    PN = 0x504E, SH = 0x5348, SL = 0x534C,
    SQ = 0x5351, SS = 0x5353, ST = 0x5354,
    TM = 0x544D, UI = 0x5549, UL = 0x554C,
    UN = 0x554E, US = 0x5553, UT = 0x5554
}

let Length = union {
    Explicit: {
        reserved: u16be = 0,
        length: u32be
    },
    Implicit: u32be
}

let Value = union {
    Sequence: SequenceValue,
    String: string,
    Numeric: NumericValue,
    Binary: Array<u8>
}

let SequenceValue = {
    items: SequenceItem[]
}

let SequenceItem = {
    tag: u32be = 0xFFFEE000,
    length: u32be,
    elements: Element[],
    delimiter: u32be = 0xFFFEE00D
}

let NumericValue = union {
    US: Array<u16be>,
    SS: Array<i16be>,
    UL: Array<u32be>,
    SL: Array<i32be>,
    FL: Array<f32be>,
    FD: Array<f64be>
}

let MetaInformation = {
    fileMetaInformationGroupLength: u32be,
    fileMetaInformationVersion: Array<u8, 2>,
    mediaStorageSOPClassUID: string,
    mediaStorageSOPInstanceUID: string,
    transferSyntaxUID: string,
    implementationClassUID: string,
    implementationVersionName: optional<string>
}

let PixelData = {
    rows: u16be,
    columns: u16be,
    bitsAllocated: u16be,
    bitsStored: u16be,
    highBit: u16be,
    pixelRepresentation: u16be,
    planarConfiguration: optional<u16be>,
    pixelData: Array<u8>
}