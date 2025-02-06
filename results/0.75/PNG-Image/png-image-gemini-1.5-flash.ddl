data PNG = PNG {
  signature :: [UInt8],
  chunks :: [Chunk]
}

data Chunk = Chunk {
  length :: UInt32,
  type :: [UInt8],
  data :: [UInt8],
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
  palette :: [RGB]
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
  transparencyData :: Maybe TransparencyData
}

data TransparencyData =
  | GrayscaleTransparency of UInt16
  | TruecolorTransparency of RGB
  | IndexedTransparency of [UInt8]

data gAMA = gAMA { gamma :: Float }
data cHRM = cHRM { whitePoint :: [Float], primaryChromaticities :: [[Float]] }
data sBIT = sBIT { significantBits :: [UInt8] }
data sRGB = sRGB { renderingIntent :: UInt8 }
data iCCP = iCCP { profileName :: String, compressedProfile :: [UInt8], compressionMethod :: UInt8 }
data iTXt = iTXt { keyword :: String, compressionFlag :: UInt8, languageTag :: String, translatedKeyword :: String, text :: String }
data tEXt = tEXt { keyword :: String, text :: String }
data zTXt = zTXt { keyword :: String, compressedText :: [UInt8] }
data bKGD = bKGD { backgroundColor :: Maybe BackgroundColor }

data BackgroundColor =
  | GrayscaleBackground of UInt16
  | TruecolorBackground of RGB
  | IndexedBackground of UInt8

data Time = Time { year :: Int16, month :: UInt8, day :: UInt8, hour :: UInt8, minute :: UInt8, second :: UInt8 }

data pHYs = pHYs { pixelsPerUnitX :: UInt32, pixelsPerUnitY :: UInt32, unitSpecifier :: UInt8 }

data oFFs = oFFs { xOffset :: Int32, yOffset :: Int32, unitSpecifier :: UInt8 }

data hIST = hIST { histogram :: [UInt32] }

data textChunk = textChunk { keyword :: String, text :: String}

data physChunk = physChunk { x :: UInt32, y :: UInt32, unit :: UInt8 }

data offsChunk = offsChunk { x :: Int32, y :: Int32, unit :: UInt8 }
