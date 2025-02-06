let ENDIAN_LITTLE = 0x4949;
let ENDIAN_BIG = 0x4D4D;

let TYPE_BYTE = 1;
let TYPE_ASCII = 2;
let TYPE_SHORT = 3;
let TYPE_LONG = 4;
let TYPE_RATIONAL = 5;
let TYPE_SBYTE = 6;
let TYPE_UNDEFINED = 7;
let TYPE_SSHORT = 8;
let TYPE_SLONG = 9;
let TYPE_SRATIONAL = 10;
let TYPE_FLOAT = 11;
let TYPE_DOUBLE = 12;

struct Rational {
    u32 numerator;
    u32 denominator;
};

struct SRational {
    i32 numerator;
    i32 denominator;
};

struct IFDEntry {
    u16 tag;
    u16 type;
    u32 count;
    u32 valueOffset;
};

struct IFD {
    u16 numEntries;
    IFDEntry[numEntries] entries;
    u32 nextIFDOffset;
};

struct TIFF {
    u16 byteOrder;
    u16 version;
    u32 firstIFDOffset;
};

bitfield TagValues {
    u8[*] byteVals if type == TYPE_BYTE;
    u8[*] asciiVals if type == TYPE_ASCII;
    u16[*] shortVals if type == TYPE_SHORT;
    u32[*] longVals if type == TYPE_LONG;
    Rational[*] rationalVals if type == TYPE_RATIONAL;
    i8[*] sbyteVals if type == TYPE_SBYTE;
    u8[*] undefinedVals if type == TYPE_UNDEFINED;
    i16[*] sshortVals if type == TYPE_SSHORT;
    i32[*] slongVals if type == TYPE_SLONG;
    SRational[*] srationalVals if type == TYPE_SRATIONAL;
    f32[*] floatVals if type == TYPE_FLOAT;
    f64[*] doubleVals if type == TYPE_DOUBLE;
};

entry TIFF;