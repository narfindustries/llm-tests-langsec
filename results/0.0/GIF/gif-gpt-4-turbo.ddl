module GIF.GPT4Turbo where

import DAEDALUS.Core

data Color = Color
  { red   :: UInt8
  , green :: UInt8
  , blue  :: UInt8
  }

data LogicalScreenDescriptor = LogicalScreenDescriptor
  { screenWidth        :: UInt16
  , screenHeight       :: UInt16
  , packedFields       :: UInt8
  , backgroundColor    :: UInt8
  , pixelAspectRatio   :: UInt8
  }

data ImageDescriptor = ImageDescriptor
  { leftPosition   :: UInt16
  , topPosition    :: UInt16
  , width          :: UInt16
  , height         :: UInt16
  , packedFields   :: UInt8
  }

data BlockType = ImageDescriptorBlock ImageDescriptor
               | ExtensionBlock UInt8
               | Trailer

data GIF = GIF
  { header                :: String 7
  , logicalScreenDesc     :: LogicalScreenDescriptor
  , globalColorTable      :: Maybe (Array Color)
  , blocks                :: Array BlockType
  }

parseColor :: Parser Color
parseColor = Color <$> u8 <*> u8 <*> u8

parseLogicalScreenDescriptor :: Parser LogicalScreenDescriptor
parseLogicalScreenDescriptor = LogicalScreenDescriptor
  <$> u16 <*> u16 <*> u8 <*> u8 <*> u8

parseImageDescriptor :: Parser ImageDescriptor
parseImageDescriptor = ImageDescriptor
  <$> u16 <*> u16 <*> u16 <*> u16 <*> u8

parseBlockType :: Parser BlockType
parseBlockType = do
  intro <- u8
  case intro of
    0x2C -> ImageDescriptorBlock <$> parseImageDescriptor
    0x21 -> ExtensionBlock <$> u8
    0x3B -> pure Trailer
    _    -> fail "Unknown block type"

parseGIF :: Parser GIF
parseGIF = GIF
  <$> string "GIF89a"
  <*> parseLogicalScreenDescriptor
  <*> optional (array parseColor)
  <*> many parseBlockType

main :: IO ()
main = parseFromFile parseGIF "input.gif"