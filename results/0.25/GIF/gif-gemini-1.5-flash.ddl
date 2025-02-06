data GIF = GIF {
  signature :: String "GIF89a",
  lsd :: LogicalScreenDescriptor,
  gct :: Maybe (Array (ColorEntry)),
  extensions :: [Extension],
  images :: [ImageData],
  trailer :: Byte 0x3B
}

data LogicalScreenDescriptor = LogicalScreenDescriptor {
  width :: Word16,
  height :: Word16,
  packedFields :: PackedFields,
  backgroundColorIndex :: Byte,
  pixelAspectRatio :: Byte
}

data PackedFields = PackedFields {
  globalColorTableFlag :: Bit,
  colorResolution :: Bits 3,
  sortFlag :: Bit,
  sizeOfGlobalColorTable :: Bits 3
}

data ColorEntry = ColorEntry {
  red :: Byte,
  green :: Byte,
  blue :: Byte
}

data ImageData = ImageData {
  imageSeparator :: Byte 0x2C,
  imageDescriptor :: ImageDescriptor,
  lct :: Maybe (Array (ColorEntry)),
  imageData :: [Byte]
}

data ImageDescriptor = ImageDescriptor {
  imageLeftPosition :: Word16,
  imageTopPosition :: Word16,
  imageWidth :: Word16,
  imageHeight :: Word16,
  packedFields :: ImagePackedFields
}

data ImagePackedFields = ImagePackedFields {
  localColorTableFlag :: Bit,
  interlaceFlag :: Bit,
  sortFlag :: Bit,
  sizeOfLocalColorTable :: Bits 3
}

data Extension = Extension {
  extensionLabel :: Byte,
  extensionData :: [Byte]
}


type Byte = Word8
type Bit = Word8
type Bits n = Word8
type Word16 = Word16
type Word32 = Word32
type Array a = [a]

