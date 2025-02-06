type RGB = struct {
    red : uint8,
    green : uint8,
    blue : uint8
};

type IHDR = struct {
    width : uint32,
    height : uint32,
    bitDepth : uint8,
    colorType : uint8,
    compressionMethod : uint8,
    filterMethod : uint8,
    interlaceMethod : uint8
};

type PLTE = struct {
    entries : [RGB]
};

type IDAT = struct {
    imageData : [uint8]
};

type IEND = struct {
};

type tEXt = struct {
    keyword : cstring,
    text : cstring
};

type zTXt = struct {
    keyword : cstring,
    compressionMethod : uint8,
    compressedText : [uint8]
};

type iTXt = struct {
    keyword : cstring,
    compressionFlag : uint8,
    compressionMethod : uint8,
    languageTag : cstring,
    translatedKeyword : cstring,
    text : cstring
};

type bKGD = struct {
    backgroundColor : [uint8]
};

type pHYs = struct {
    pixelsPerUnitX : uint32,
    pixelsPerUnitY : uint32,
    unitSpecifier : uint8
};

type sBIT = struct {
    significantBits : [uint8]
};

type sRGB = struct {
    renderingIntent : uint8
};

type tIME = struct {
    year : uint16,
    month : uint8,
    day : uint8,
    hour : uint8,
    minute : uint8,
    second : uint8
};

type gAMA = struct {
    gamma : uint32
};

type cHRM = struct {
    whitePointX : uint32,
    whitePointY : uint32,
    redX : uint32,
    redY : uint32,
    greenX : uint32,
    greenY : uint32,
    blueX : uint32,
    blueY : uint32
};

type hIST = struct {
    frequencies : [uint16]
};

type sPLTEntry = struct {
    red : uint16,
    green : uint16,
    blue : uint16,
    alpha : uint16,
    frequency : uint16
};

type sPLT = struct {
    paletteName : cstring,
    sampleDepth : uint8,
    paletteEntries : [sPLTEntry]
};

type iCCP = struct {
    profileName : cstring,
    compressionMethod : uint8,
    compressedProfile : [uint8]
};

type PNGChunk = struct {
    length : uint32,
    chunkType : string(4),
    data : [uint8] @length,
    crc : uint32
};

type PNGFile = struct {
    signature : [uint8] @8,
    chunks : [PNGChunk]
};