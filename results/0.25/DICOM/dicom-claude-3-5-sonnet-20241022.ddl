struct DICOM {
    preamble: u8[128];
    prefix: ascii[4];
    meta_information: MetaInformation;
    dataset: DataElement[];
};

struct MetaInformation {
    file_meta_length: u32;
    file_meta_version: u8[2];
    media_storage_sop_class_uid: string;
    media_storage_sop_instance_uid: string;
    transfer_syntax_uid: string;
    implementation_class_uid: string;
    implementation_version_name: string?;
    source_application_entity_title: string?;
    private_information_creator_uid: string?;
    private_information: u8[]?;
};

struct DataElement {
    tag: Tag;
    vr: VR?;
    value_length: ValueLength;
    value: Value;
};

struct Tag {
    group: u16;
    element: u16;
};

enum u16 VR {
    AE = 0x4145,
    AS = 0x4153,
    AT = 0x4154,
    CS = 0x4353,
    DA = 0x4441,
    DS = 0x4453,
    DT = 0x4454,
    FL = 0x464C,
    FD = 0x4644,
    IS = 0x4953,
    LO = 0x4C4F,
    LT = 0x4C54,
    OB = 0x4F42,
    OD = 0x4F44,
    OF = 0x4F46,
    OL = 0x4F4C,
    OV = 0x4F56,
    OW = 0x4F57,
    PN = 0x504E,
    SH = 0x5348,
    SL = 0x534C,
    SQ = 0x5351,
    SS = 0x5353,
    ST = 0x5354,
    SV = 0x5356,
    TM = 0x544D,
    UC = 0x5543,
    UI = 0x5549,
    UL = 0x554C,
    UN = 0x554E,
    UR = 0x5552,
    US = 0x5553,
    UT = 0x5554,
    UV = 0x5556
};

struct ValueLength {
    length: u32 if vr in [OB, OW, SQ, UN] else u16;
};

union Value(vr: VR) {
    SQ: Sequence;
    OB: u8[value_length];
    OW: u16[value_length/2];
    US: u16[value_length/2];
    UL: u32[value_length/4];
    SS: i16[value_length/2];
    SL: i32[value_length/4];
    FL: f32[value_length/4];
    FD: f64[value_length/8];
    default: string;
};

struct Sequence {
    items: SequenceItem[];
};

struct SequenceItem {
    item_tag: Tag;
    item_length: u32;
    item_data: DataElement[];
    item_delimiter_tag: Tag?;
};