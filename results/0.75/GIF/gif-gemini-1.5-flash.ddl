module GIF.GifGemini1 where

import Daedalus.Type.AST

-- Data types for GIF image

data Color = Color { red :: Word8, green :: Word8, blue :: Word8 }

data Image = Image { width :: Word16, height :: Word16, colors :: [Color], pixels :: [Word8] }

-- Parsing function for GIF image

parseGIF :: forall m . Monad m => Parser m Image

parseGIF = do
  width <- word16
  height <- word16
  numColors <- word8
  colors <- replicateM numColors (Color <$> word8 <*> word8 <*> word8)
  pixels <- replicateM (fromIntegral width * fromIntegral height) word8
  return $ Image width height colors pixels


-- Serialization function for GIF image

serializeGIF :: Image -> [Word8]

serializeGIF image =
  word16ToBytes (width image) ++
  word16ToBytes (height image) ++
  [fromIntegral $ length $ colors image] ++
  concatMap colorToBytes (colors image) ++
  pixels image

word16ToBytes :: Word16 -> [Word8]
word16ToBytes w = [fromIntegral (w `shiftR` 8), fromIntegral (w .&. 0xFF)]

colorToBytes :: Color -> [Word8]
colorToBytes (Color r g b) = [r, g, b]

