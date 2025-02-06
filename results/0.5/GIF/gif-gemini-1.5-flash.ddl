data GIF = GIF {
  signature :: Constant "GIF89a",
  logicalScreenWidth :: Word16,
  logicalScreenHeight :: Word16,
  packedFields :: PackedFields,
  backgroundColorIndex :: Word8,
  pixelAspectRatio :: Word8,
  globalColorTable :: Maybe (Array RGB),
  images :: [Image],
  trailer :: Constant 0x3B
}

data PackedFields = PackedFields {
  globalColorTableFlag :: Bit,
  colorResolution :: Bits 3,
  sortFlag :: Bit,
  sizeOfGlobalColorTable :: Bits 3
}

data RGB = RGB {
  red :: Word8,
  green :: Word8,
  blue :: Word8
}

data Image = Image {
  imageSeparator :: Constant 0x2C,
  imageLeftPosition :: Word16,
  imageTopPosition :: Word16,
  imageWidth :: Word16,
  imageHeight :: Word16,
  packedFields :: ImagePackedFields,
  localColorTable :: Maybe (Array RGB),
  lzwMinimumCodeSize :: Word8,
  compressedImageData :: Bytes
}

data ImagePackedFields = ImagePackedFields {
  localColorTableFlag :: Bit,
  interlaceFlag :: Bit,
  sortFlag :: Bit,
  sizeOfLocalColorTable :: Bits 3
}
