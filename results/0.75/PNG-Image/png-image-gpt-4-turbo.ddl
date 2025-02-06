module PNG;

type PNGSignature = struct {
  signature : uint64be const : 0x89504E470D0A1A0A;
};

type ChunkHeader = struct {
  length : uint32be;
  type : string(4);
};

type IHDR = struct {
  width : uint32be;
  height : uint32be;
  bitDepth : uint8;
  colorType : uint8;
  compressionMethod : uint8;
  filterMethod : uint8;
  interlaceMethod : uint8;
};

type PLTE = array[struct {
  red : uint8;
  green : uint8;
  blue : uint8;
}] depends on context['PLTE size'];

type IDAT = struct {
  data : bytes;
};

type IEND = struct {
};

type tEXt = struct {
  keyword : zstring;
  text : zstring;
};

type zTXt = struct {
  keyword : zstring;
  compressionMethod : uint8;
  compressedText : bytes;
};

type iTXt = struct {
  keyword : zstring;
  compressionFlag : uint8;
  compressionMethod : uint8;
  languageTag : zstring;
  translatedKeyword : zstring;
  text : zstring;
};

type bKGD = switch (context['ColorType']) {
  case 0, 4 => struct { gray : uint16be; };
  case 2, 6 => struct { red : uint16be; green : uint16be; blue : uint16be; };
  case 3 => struct { paletteIndex : uint8; };
};

type pHYs = struct {
  pixelsPerUnitX : uint32be;
  pixelsPerUnitY : uint32be;
  unitSpecifier : uint8;
};

type sBIT = switch (context['ColorType']) {
  case 0 => struct { gray : uint8; };
  case 2, 3 => struct { red : uint8; green : uint8; blue : uint8; };
  case 4 => struct { gray : uint8; alpha : uint8; };
  case 6 => struct { red : uint8; green : uint8; blue : uint8; alpha : uint8; };
};

type sRGB = struct {
  renderingIntent : uint8;
};

type tIME = struct {
  year : uint16be;
  month : uint8;
  day : uint8;
  hour : uint8;
  minute : uint8;
  second : uint8;
};

type gAMA = struct {
  gamma : uint32be;
};

type UnknownChunk = struct {
  data : bytes depends on context['length'];
};

type PNGChunk = struct {
  header : ChunkHeader;
  body : choice depends on header.type {
    case "IHDR" => IHDR;
    case "PLTE" => PLTE;
    case "IDAT" => IDAT;
    case "IEND" => IEND;
    case "tEXt" => tEXt;
    case "zTXt" => zTXt;
    case "iTXt" => iTXt;
    case "bKGD" => bKGD;
    case "pHYs" => pHYs;
    case "sBIT" => sBIT;
    case "sRGB" => sRGB;
    case "tIME" => tIME;
    case "gAMA" => gAMA;
    default => UnknownChunk;
  };
  crc : uint32be;
};

type PNGFile = struct {
  signature : PNGSignature;
  chunks : array[PNGChunk];
};

main : PNGFile;