data GIF89a = GIF89a
  { signature : String 6
  , logicalScreenDescriptor : LogicalScreenDescriptor
  , globalColorTable : Maybe GlobalColorTable
  , images : [ImageDescriptor]
  , trailer : Trailer
  }

data LogicalScreenDescriptor = LogicalScreenDescriptor
  { width : Word16
  , height : Word16
  , packedFields : PackedFields
  , backgroundColorIndex : Byte
  , pixelAspectRatio : Byte
  }

data PackedFields = PackedFields
  { globalColorTableFlag : Bit
  , colorResolution : Bits3
  , sortFlag : Bit
  , sizeOfGlobalColorTable : Bits3
  }

data GlobalColorTable = GlobalColorTable
  { entries : [ColorEntry]
  }

data ColorEntry = ColorEntry
  { red : Byte
  , green : Byte
  , blue : Byte
  }

data ImageDescriptor = ImageDescriptor
  { imageSeparator : Byte
  , imageLeftPosition : Word16
  , imageTopPosition : Word16
  , imageWidth : Word16
  , imageHeight : Word16
  , packedFields : PackedFieldsImage
  , localColorTable : Maybe LocalColorTable
  , imageData : [Byte]
  }

data PackedFieldsImage = PackedFieldsImage
  { localColorTableFlag : Bit
  , interlaceFlag : Bit
  , sortFlag : Bit
  , sizeOfLocalColorTable : Bits3
  }

data LocalColorTable = LocalColorTable
  { entries : [ColorEntry]
  }

data Trailer = Trailer { trailer : Byte }

data Bits3 = Bits3 Word8

data Bit = Bit Bool

data Word16 = Word16 Word16

data Byte = Byte Word8
