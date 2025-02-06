module JPEG

import Data.Maybe
import Data.List
import Data.Word

data Marker = SOI | APPn Word8 Bytes | DQT Word8 Word8 [Word8] | DHT Word8 Word8 [(Word8,Word8)] | DRI Word16 | SOS [ComponentSpecifier] Word8 Word8 Word8 | COM Bytes | EOI | RSTn Word8
  deriving (Show, Eq)

data ComponentSpecifier = ComponentSpecifier Word8 Word8 Word8
  deriving (Show, Eq)

data JPEG = JPEG [Marker]
  deriving (Show, Eq)

parseJPEG :: Bytes -> Maybe JPEG
parseJPEG bs = Just $ JPEG (parseMarkers bs)

parseMarkers :: Bytes -> [Marker]
parseMarkers [] = []
parseMarkers (0xFF : marker : rest) =
  case marker of
    0xD8 -> SOI : parseMarkers rest
    0xDA ->
      let (comps, rest1) = parseComponentSpecifiers rest
          (spectralStart, rest2) = parseWord8 rest1
          (spectralEnd, rest3) = parseWord8 rest3
          (approx, rest4) = parseWord8 rest3
       in SOS comps spectralStart spectralEnd approx : parseMarkers rest4
    0xDB ->
      let (tableId, rest1) = parseWord8 rest
          (precision, rest2) = parseWord8 rest1
          (quant, rest3) = parseQuantTable rest2 precision
       in DQT tableId precision quant : parseMarkers rest3
    0xC4 ->
      let (tableId, rest1) = parseWord8 rest
          (tableClass, rest2) = parseWord8 rest1
          (huffmanTable, rest3) = parseHuffmanTable rest2
       in DHT tableId tableClass huffmanTable : parseMarkers rest3
    0xDD ->
      let (restartInterval, rest1) = parseWord16 rest
       in DRI restartInterval : parseMarkers rest1
    0xFE ->
      let (comment, rest1) = parseComment rest
       in COM comment : parseMarkers rest1
    0xD9 -> [EOI]
    0xD0 .. 0xD7 ->
      let (restartMarker, rest1) = parseWord8 rest
       in RSTn restartMarker : parseMarkers rest1
    0xE0 .. 0xEF ->
      let (appn, rest1) = parseWord8 rest
          (data, rest2) = parseBytesUntil 0xFF rest1
       in APPn appn data : parseMarkers rest2
    _ -> parseMarkers rest
parseMarkers _ = []


parseComponentSpecifiers :: Bytes -> ([ComponentSpecifier], Bytes)
parseComponentSpecifiers bs = ([], bs) --Placeholder


parseQuantTable :: Bytes -> Word8 -> ([Word8], Bytes)
parseQuantTable bs precision = ([], bs) --Placeholder


parseHuffmanTable :: Bytes -> ([(Word8, Word8)], Bytes)
parseHuffmanTable bs = ([], bs) --Placeholder


parseWord8 :: Bytes -> (Word8, Bytes)
parseWord8 (x : xs) = (x, xs)
parseWord8 [] = error "Unexpected end of byte stream"


parseWord16 :: Bytes -> (Word16, Bytes)
parseWord16 (x : y : xs) = ((fromIntegral x `shiftL` 8) .|. fromIntegral y, xs)
parseWord16 _ = error "Unexpected end of byte stream"


parseBytesUntil :: Word8 -> Bytes -> (Bytes, Bytes)
parseBytesUntil marker bs = ([], bs) --Placeholder


parseComment :: Bytes -> (Bytes, Bytes)
parseComment bs = ([], bs) --Placeholder

