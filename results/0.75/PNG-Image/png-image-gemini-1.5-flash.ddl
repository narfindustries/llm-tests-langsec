{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module PNGImage where

import Daedalus.Type.AST
import Daedalus.PP
import Daedalus.Interp

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data PNGHeader = PNGHeader {
  signature :: [Byte],
  ihdr :: IHDR
} deriving (Show, Generic, ToJSON, FromJSON)

data IHDR = IHDR {
  width :: Integer,
  height :: Integer,
  bitDepth :: Integer,
  colorType :: Integer,
  compressionMethod :: Integer,
  filterMethod :: Integer,
  interlaceMethod :: Integer
} deriving (Show, Generic, ToJSON, FromJSON)


pngHeader :: Daedalus a PNGHeader
pngHeader = do
  signature' <- bytes 8
  when (signature' /= [0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A]) $ fail "Invalid PNG signature"
  ihdr' <- ihdr
  return $ PNGHeader signature' ihdr'

ihdr :: Daedalus a IHDR
ihdr = do
  width' <- uint32
  height' <- uint32
  bitDepth' <- uint8
  colorType' <- uint8
  compressionMethod' <- uint8
  filterMethod' <- uint8
  interlaceMethod' <- uint8
  return $ IHDR width' height' bitDepth' colorType' compressionMethod' filterMethod' interlaceMethod'


uint32 :: Daedalus a Integer
uint32 = do
  x <- bytes 4
  return $ (fromIntegral (x!!0) * 2^24) + (fromIntegral (x!!1) * 2^16) + (fromIntegral (x!!2) * 2^8) + fromIntegral (x!!3)

uint8 :: Daedalus a Integer
uint8 = fromIntegral <$> byte

byte :: Daedalus a Integer
byte = fromIntegral <$> get


type Byte = Word8

main :: IO ()
main = do
  let p = compile $ choice [pngHeader]
  print (pp p)

  --Example usage (requires a PNG file)
  --let result = runParser p "path/to/your/image.png"
  --print result
