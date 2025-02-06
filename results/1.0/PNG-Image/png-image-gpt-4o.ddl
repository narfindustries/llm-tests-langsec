alias uint32 = U32be;
alias uint16 = U16be;
alias byte = U8;
alias text(n) = Str(n);
alias bytes(n) = Bytes(n);

type PngSignature = struct {
    magic : bytes(8);
};

type ChunkHeader = struct {
    length : uint32;
    type : text(4);
};

type IhdrChunk = struct {
    width : uint32;
    height : uint32;
    bit_depth : byte;
    color_type : byte;
    compression_method : byte;
    filter_method : byte;
    interlace_method : byte;
};

type PlteEntry = struct {
    red : byte;
    green : byte;
    blue : byte;
};

type PlteChunk = struct {
    entries : PlteEntry[length / 3];
};

type IdatChunk = struct {
    data : bytes[length];
};

type IendChunk = struct {};

type TextualData = struct {
    keyword : bytes[$];
    text : bytes[$];
};

type CompressedTextualData = struct {
    keyword : bytes[$];
    compression_method : byte;
    compressed_text : bytes[$];
};

type InternationalTextualData = struct {
    keyword : bytes[$];
    compression_flag : byte;
    compression_method : byte;
    language_tag : bytes[$];
    translated_keyword : bytes[$];
    text : bytes[$];
};

type BkgdChunk = struct {
    data : bytes[length];
};

type Chrinfo = struct {
    white_x : uint32;
    white_y : uint32;
    red_x : uint32;
    red_y : uint32;
    green_x : uint32;
    green_y : uint32;
    blue_x : uint32;
    blue_y : uint32;
};

type Gamma = struct {
    gamma_value : uint32;
};

type SrgbChunk = struct {
    rendering_intent : byte;
};

type TrnsChunk = struct {
    data : bytes[length];
};

type PhysChunk = struct {
    pixels_per_unit_x : uint32;
    pixels_per_unit_y : uint32;
    unit_specifier : byte;
};

type SbitChunk = struct {
    data : bytes[length];
};

type HistChunk = struct {
    frequencies : uint16[length / 2];
};

type IccpChunk = struct {
    profile_name : bytes[$];
    compression_method : byte;
    compressed_profile : bytes[$];
};

type PngChunk = struct {
    header : ChunkHeader;
    data : switch(header.type) {
        case "IHDR": IhdrChunk;
        case "PLTE": PlteChunk;
        case "IDAT": IdatChunk;
        case "IEND": IendChunk;
        case "tEXt": TextualData;
        case "zTXt": CompressedTextualData;
        case "iTXt": InternationalTextualData;
        case "bKGD": BkgdChunk;
        case "cHRM": Chrinfo;
        case "gAMA": Gamma;
        case "sRGB": SrgbChunk;
        case "tRNS": TrnsChunk;
        case "pHYs": PhysChunk;
        case "sBIT": SbitChunk;
        case "hIST": HistChunk;
        case "iCCP": IccpChunk;
        default: bytes[header.length];
    };
    crc : uint32;
};

type PngFile = struct {
    signature : PngSignature;
    chunks : PngChunk[$];
};