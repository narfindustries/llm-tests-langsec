data PNG = Signature { sig :: [UInt8 8] } :+ Chunks { chunks :: [Chunk] }

data Chunk = IHDR { width :: UInt32, height :: UInt32, bitDepth :: UInt8, colorType :: UInt8, compressionMethod :: UInt8, filterMethod :: UInt8, interlaceMethod :: UInt8 }
          | PLTE { palette :: [RGB] }
          | IDAT { data :: [UInt8] }
          | IEND {}
          | tRNS { transparency :: [UInt8] }
          | cHRM { whitePointX :: UInt32, whitePointY :: UInt32, redX :: UInt32, redY :: UInt32, greenX :: UInt32, greenY :: UInt32, blueX :: UInt32, blueY :: UInt32 }
          | gAMA { gamma :: UInt32 }
          | iCCP { profileName :: String, compressionMethod :: UInt8, compressedProfile :: [UInt8] }
          | sBIT { significantBits :: [UInt8] }
          | sRGB { renderingIntent :: UInt8 }
          | bKGD { backgroundColor :: [UInt8] }
          | hIST { histogram :: [UInt32] }
          | pHYs { pixelsPerUnitX :: UInt32, pixelsPerUnitY :: UInt32, unitSpecifier :: UInt8 }
          | tEXt { keyword :: String, text :: String }
          | zTXt { keyword :: String, compressionMethod :: UInt8, compressedText :: [UInt8] }
          | iTXt { keyword :: String, compressionFlag :: UInt8, compressionMethod :: UInt8, languageTag :: String, translatedKeyword :: String, text :: String }
          | tIME { year :: UInt16, month :: UInt8, day :: UInt8, hour :: UInt8, minute :: UInt8, second :: UInt8 }

data RGB = RGB { r :: UInt8, g :: UInt8, b :: UInt8 }
