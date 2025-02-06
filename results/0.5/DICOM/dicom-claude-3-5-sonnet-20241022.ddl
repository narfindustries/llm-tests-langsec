struct DicomFile {
    DicomHeader header;
    MetaInformation meta_info;
    DataSet data_set;
};

struct DicomHeader {
    u8[128] preamble;
    char[4] magic = "DICM";
};

struct MetaInformation {
    MetaElement[] elements;
};

struct MetaElement {
    u16 group = 0x0002;
    u16 element;
    VR vr;
    ElementLength length;
    Value value;
};

struct DataSet {
    Element[] elements;
};

struct Element {
    Tag tag;
    optional VR vr;
    ElementLength length;
    Value value;
};

struct Tag {
    u16 group;
    u16 element;
};

enum VR {
    AE, AS, AT, CS, DA, DS, DT, FL, FD, IS,
    LO, LT, OB, OF, OW, PN, SH, SL, SQ, SS,
    ST, TM, UI, UL, UN, US, UT
};

union ElementLength {
    u32 long if vr in [OB, OW, OF, SQ, UT, UN];
    u16 short;
};

union Value {
    SequenceValue sequence if vr == SQ;
    PixelData pixels if tag.group == 0x7FE0 && tag.element == 0x0010;
    char[] string if vr in [AE, AS, CS, DA, DS, DT, IS, LO, LT, PN, SH, ST, TM, UI, UT];
    NumericValue numeric;
    u8[] raw;
};

struct SequenceValue {
    SequenceItem[] items;
    optional SequenceDelimiter delimiter if length == 0xFFFFFFFF;
};

struct SequenceItem {
    ItemTag tag;
    u32 length;
    DataSet dataset;
    optional ItemDelimiter delimiter if length == 0xFFFFFFFF;
};

struct ItemTag {
    u16 group = 0xFFFE;
    u16 element = 0xE000;
};

struct ItemDelimiter {
    u16 group = 0xFFFE;
    u16 element = 0xE00D;
    u32 length = 0;
};

struct SequenceDelimiter {
    u16 group = 0xFFFE;
    u16 element = 0xE0DD;
    u32 length = 0;
};

union PixelData {
    Fragment[] fragments if transfer_syntax_encapsulated;
    u8[] raw;
};

struct Fragment {
    ItemTag tag;
    u32 length;
    u8[length] data;
};

union NumericValue {
    f32[length/4] fl if vr == FL;
    f64[length/8] fd if vr == FD;
    i32[length/4] sl if vr == SL;
    i16[length/2] ss if vr == SS;
    u32[length/4] ul if vr == UL;
    u16[length/2] us if vr == US;
};