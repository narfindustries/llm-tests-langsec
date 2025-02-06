data GIF = GIF {
  signature :: String "GIF89a",
  logicalScreenDescriptor :: LogicalScreenDescriptor,
  globalColorTable :: Maybe (Vector ColorEntry 768),
  extensions :: [ExtensionBlock],
  images :: [Image],
  trailer :: String 1
}

data LogicalScreenDescriptor = LogicalScreenDescriptor {
  width :: Word16,
  height :: Word16,
  packedFields :: PackedFields,
  backgroundColorIndex :: Word8,
  pixelAspectRatio :: Word8
}

data PackedFields = PackedFields {
  globalColorTableFlag :: Bit,
  colorResolution :: Bits 3,
  sortFlag :: Bit,
  sizeOfGlobalColorTable :: Bits 3
}

data ColorEntry = ColorEntry {
  red :: Word8,
  green :: Word8,
  blue :: Word8
}

data Image = Image {
  imageSeparator :: String 1,
  imageLeftPosition :: Word16,
  imageTopPosition :: Word16,
  imageWidth :: Word16,
  imageHeight :: Word16,
  packedFields :: ImagePackedFields,
  localColorTable :: Maybe (Vector ColorEntry 768),
  imageData :: [Word8]
}

data ImagePackedFields = ImagePackedFields {
  localColorTableFlag :: Bit,
  interlaceFlag :: Bit,
  sortFlag :: Bit,
  reserved :: Bits 2,
  sizeOfLocalColorTable :: Bits 3
}

data ExtensionBlock = ExtensionBlock {
  label :: Word8,
  data :: [Word8]
}
