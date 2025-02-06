PNG = {
  Signature: [0x89, 'P', 'N', 'G', '\r', '\n', 0x1A, '\n'],
  Chunks: many(Chunk)
}

Chunk = {
  Length: uint32be,
  Type: ChunkType,
  Data: switch (Type) {
    'IHDR' => IHDR,
    'PLTE' => PLTE,
    'IDAT' => IDAT,
    'IEND' => IEND,
    'tEXt' => tEXt,
    'zTXt' => zTXt,
    'iTXt' => iTXt,
    'bKGD' => bKGD,
    'pHYs' => pHYs,
    'tIME' => tIME,
    'gAMA' => gAMA,
    'cHRM' => cHRM,
    'sRGB' => sRGB,
    'iCCP' => iCCP,
    'sBIT' => sBIT,
    'hIST' => hIST,
    'tRNS' => tRNS,
    _ => bytes(Length)
  },
  CRC: uint32be
}

ChunkType = bytes(4)

IHDR = {
  Width: uint32be,
  Height: uint32be,
  BitDepth: uint8,
  ColorType: uint8,
  CompressionMethod: uint8,
  FilterMethod: uint8,
  InterlaceMethod: uint8
}

PLTE = many({
  Red: uint8,
  Green: uint8,
  Blue: uint8
})

IDAT = bytes(Length)

IEND = bytes(0)

tEXt = {
  Keyword: cstring,
  Text: bytes(Length - Keyword.length - 1)
}

zTXt = {
  Keyword: cstring,
  CompressionMethod: uint8,
  CompressedText: bytes(Length - Keyword.length - 2)
}

iTXt = {
  Keyword: cstring,
  CompressionFlag: uint8,
  CompressionMethod: uint8,
  LanguageTag: cstring,
  TranslatedKeyword: cstring,
  Text: switch (CompressionFlag) {
    0 => bytes(Length - Keyword.length - LanguageTag.length - TranslatedKeyword.length - 3),
    1 => bytes(Length - Keyword.length - LanguageTag.length - TranslatedKeyword.length - 4)
  }
}

bKGD = switch (IHDR.ColorType) {
  0 | 4 => { Gray: uint16be },
  2 | 6 => { Red: uint16be, Green: uint16be, Blue: uint16be },
  3 => { Index: uint8 }
}

pHYs = {
  PixelsPerUnitX: uint32be,
  PixelsPerUnitY: uint32be,
  UnitSpecifier: uint8
}

tIME = {
  Year: uint16be,
  Month: uint8,
  Day: uint8,
  Hour: uint8,
  Minute: uint8,
  Second: uint8
}

gAMA = {
  Gamma: uint32be
}

cHRM = {
  WhitePointX: uint32be,
  WhitePointY: uint32be,
  RedX: uint32be,
  RedY: uint32be,
  GreenX: uint32be,
  GreenY: uint32be,
  BlueX: uint32be,
  BlueY: uint32be
}

sRGB = {
  RenderingIntent: uint8
}

iCCP = {
  ProfileName: cstring,
  CompressionMethod: uint8,
  CompressedProfile: bytes(Length - ProfileName.length - 2)
}

sBIT = switch (IHDR.ColorType) {
  0 => { Gray: uint8 },
  2 => { Red: uint8, Green: uint8, Blue: uint8 },
  3 => { Red: uint8, Green: uint8, Blue: uint8 },
  4 => { Gray: uint8, Alpha: uint8 },
  6 => { Red: uint8, Green: uint8, Blue: uint8, Alpha: uint8 }
}

hIST = many(uint16be)

tRNS = switch (IHDR.ColorType) {
  0 => { Gray: uint16be },
  2 => { Red: uint16be, Green: uint16be, Blue: uint16be },
  3 => many(uint8),
  4 => { Gray: uint16be },
  6 => { Red: uint16be, Green: uint16be, Blue: uint16be }
}

cstring = take_while(b => b != 0) ++ [0]

bytes(n) = take(n)

uint8 = take(1)

uint16be = take(2)

uint32be = take(4)

many(T) = take_while(b => b != undefined) .map(b => T(b))