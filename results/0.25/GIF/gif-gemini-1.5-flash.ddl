module GIF.GifGemini1 where

import Daedalus.Type.AST

data GifImage = GifImage {
  width :: Integer,
  height :: Integer,
  globalPalette :: Maybe (PaletteEntry Integer),
  images :: [Image]
}

data Image = Image {
  left :: Integer,
  top :: Integer,
  width :: Integer,
  height :: Integer,
  localPalette :: Maybe (PaletteEntry Integer),
  interlace :: Bool,
  lzwMinCodeSize :: Integer,
  imageData :: [Integer]
}

data PaletteEntry a = PaletteEntry {
  red :: a,
  green :: a,
  blue :: a
}

instance Semigroup (PaletteEntry a) where
  (PaletteEntry r1 g1 b1) <> (PaletteEntry r2 g2 b2) = PaletteEntry (r1 <> r2) (g1 <> g2) (b1 <> b2)

instance Monoid (PaletteEntry a) where
  mempty = PaletteEntry 0 0 0

-- Helper functions (add more as needed)
readInteger :: forall a. (Monad m, Read a) => m a
readInteger = read <$> getLine

readPaletteEntry :: forall m a. (Monad m, Read a) => m (PaletteEntry a)
readPaletteEntry = do
  r <- readInteger
  g <- readInteger
  b <- readInteger
  return $ PaletteEntry r g b

readGifImage :: forall m. Monad m => m GifImage
readGifImage = do
  width <- readInteger
  height <- readInteger
  globalPaletteMaybe <- (Just <$> readPaletteEntry) <|> pure Nothing
  numImages <- readInteger
  images <- replicateM numImages readImage
  return $ GifImage width height globalPaletteMaybe images

readImage :: forall m. Monad m => m Image
readImage = do
  left <- readInteger
  top <- readInteger
  width <- readInteger
  height <- readInteger
  localPaletteMaybe <- (Just <$> readPaletteEntry) <|> pure Nothing
  interlace <- readBool
  lzwMinCodeSize <- readInteger
  imageDataLen <- readInteger
  imageData <- replicateM imageDataLen readInteger
  return $ Image left top width height localPaletteMaybe interlace lzwMinCodeSize imageData

readBool :: forall m. Monad m => m Bool
readBool = do
  i <- readInteger
  return (i /= 0)

main :: forall m. Monad m => m ()
main = do
  gif <- readGifImage
  putStrLn $ show gif
