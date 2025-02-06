type UInt32be = Bits32be;
type UInt16be = Bits16be;
type UInt8 = Bits8;

type RGB = struct {
    red: UInt8;
    green: UInt8;
    blue: UInt8;
};

type IHDR = struct {
    width: UInt32be;
    height: UInt32be;
    bitDepth: UInt8;
    colorType: UInt8;
    compressionMethod: UInt8;
    filterMethod: UInt8;
    interlaceMethod: UInt8;
};

type PLTE = struct {
    entries: Array<RGB>;
};

type IDAT = struct {
    data: Array<UInt8>;
};

type IEND = struct {
    // No fields
};

type tEXt = struct {
    keyword: Array<UInt8>;
    nullSeparator: UInt8;
    text: Array<UInt8>;
};

type zTXt = struct {
    keyword: Array<UInt8>;
    nullSeparator: UInt8;
    compressionMethod: UInt8;
    compressedText: Array<UInt8>;
};

type bKGD = struct {
    backgroundColor: Array<UInt8>;
};

type pHYs = struct {
    pixelsPerUnitXAxis: UInt32be;
    pixelsPerUnitYAxis: UInt32be;
    unitSpecifier: UInt8;
};

type tIME = struct {
    year: UInt16be;
    month: UInt8;
    day: UInt8;
    hour: UInt8;
    minute: UInt8;
    second: UInt8;
};

type PNGChunk = struct {
    length: UInt32be;
    type: UInt32be;
    data: Array<UInt8> = length;
    crc: UInt32be;
};

type PNGImage = struct {
    signature: Bits64be;
    chunks: Array<PNGChunk>;
};

rule PNGChunkParser(chunk: PNGChunk) = switch UInt32be(chunk.type) {
    0x49484452 => parse IHDR(chunk.data),   // 'IHDR'
    0x504C5445 => parse PLTE(chunk.data),   // 'PLTE'
    0x49444154 => parse IDAT(chunk.data),   // 'IDAT'
    0x49454E44 => parse IEND(chunk.data),   // 'IEND'
    0x74455874 => parse tEXt(chunk.data),   // 'tEXt'
    0x7A545874 => parse zTXt(chunk.data),   // 'zTXt'
    0x624B4744 => parse bKGD(chunk.data),   // 'bKGD'
    0x70485973 => parse pHYs(chunk.data),   // 'pHYs'
    0x74494D45 => parse tIME(chunk.data),   // 'tIME'
    _ => {} // Handle other types or unknown types
};

rule PNGImageParser(input: Array<UInt8>) = parse PNGImage(input);