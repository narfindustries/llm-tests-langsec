data PNG = PNG {
  signature :: [UInt8] {length: 8, value: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]},
  chunks :: [Chunk]
}

data Chunk = Chunk {
  length :: UInt32,
  type :: String {length: 4},
  data :: [UInt8] {length: length},
  crc :: UInt32
}

data IHDR = IHDR {
  width :: UInt32,
  height :: UInt32,
  bitDepth :: UInt8,
  colorType :: UInt8,
  compressionMethod :: UInt8,
  filterMethod :: UInt8,
  interlaceMethod :: UInt8
}

data PLTE = PLTE {
  entries :: [RGB]
}

data RGB = RGB {
  red :: UInt8,
  green :: UInt8,
  blue :: UInt8
}

data IDAT = IDAT {
  data :: [UInt8]
}

data IEND = IEND {}

data tRNS = tRNS {
  transparency :: [UInt8]
}

data cHRM = cHRM {
  whitePointX :: UInt32,
  whitePointY :: UInt32,
  redX :: UInt32,
  redY :: UInt32,
  greenX :: UInt32,
  greenY :: UInt32,
  blueX :: UInt32,
  blueY :: UInt32
}

data gAMA = gAMA {
  gamma :: UInt32
}

data iCCP = iCCP {
  profileName :: String,
  compressionMethod :: UInt8,
  compressedProfile :: [UInt8]
}

data sBIT = sBIT {
  significantBits :: [UInt8]
}

data sRGB = sRGB {
  renderingIntent :: UInt8
}

data bKGD = bKGD {
  backgroundColor :: [UInt8]
}

data hIST = hIST {
  histogram :: [UInt32]
}

data pHYs = pHYs {
  pixelsPerUnitX :: UInt32,
  pixelsPerUnitY :: UInt32,
  unitSpecifier :: UInt8
}

data tEXt = tEXt {
  keyword :: String,
  text :: String
}

data zTXt = zTXt {
  keyword :: String,
  compressionMethod :: UInt8,
  compressedText :: [UInt8]
}

data iTXt = iTXt {
  keyword :: String,
  compressionFlag :: UInt8,
  compressionMethod :: UInt8,
  languageTag :: String,
  translatedKeyword :: String,
  text :: String
}

data tIME = tIME {
  year :: UInt16,
  month :: UInt8,
  day :: UInt8,
  hour :: UInt8,
  minute :: UInt8,
  second :: UInt8
}
