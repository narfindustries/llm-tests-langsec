{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GIF where

import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Value
import Daedalus.PP
import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Expr
import Daedalus.Core.Var
import Daedalus.Core.Type
import Daedalus.Driver
import Daedalus.Parser.Monad
import Daedalus.Compiler
import Daedalus.Interp
import Daedalus.Pretty
import Daedalus.RTS.Input
import Daedalus.RTS.Output

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Binary (Binary, get, put)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Serialize (Serialize, get, put)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)


data GIFHeader = GIFHeader
  { version :: ByteString
  , width :: Word16
  , height :: Word16
  , flags :: Word8
  , backgroundColor :: Word8
  , aspectRatio :: Word8
  } deriving (Show, Generic, Binary, Serialize)


data GIFScreenDescriptor = GIFScreenDescriptor
  { width :: Word16
  , height :: Word16
  , flags :: Word8
  , backgroundColorIndex :: Word8
  , pixelAspectRatio :: Word8
  } deriving (Show, Generic, Binary, Serialize)


data GIFImageDescriptor = GIFImageDescriptor
  { left :: Word16
  , top :: Word16
  , width :: Word16
  , height :: Word16
  , flags :: Word8
  , localColorTableFlag :: Bool
  , interlaceFlag :: Bool
  , sortFlag :: Bool
  , localColorTableSize :: Word8
  } deriving (Show, Generic, Binary, Serialize)


data GIFGraphicControlExtension = GIFGraphicControlExtension
  { disposalMethod :: Word8
  , userInputFlag :: Bool
  , transparencyFlag :: Bool
  , delayTime :: Word16
  , transparentColorIndex :: Word8
  } deriving (Show, Generic, Binary, Serialize)


data GIFCommentExtension = GIFCommentExtension
  { comment :: ByteString
  } deriving (Show, Generic, Binary, Serialize)


data GIFPlainTextExtension = GIFPlainTextExtension
  { textGridLeft :: Word16
  , textGridTop :: Word16
  , textGridWidth :: Word16
  , textGridHeight :: Word16
  , cellWidth :: Word8
  , cellHeight :: Word8
  , textForegroundColor :: Word8
  , textBackgroundColor :: Word8
  , text :: ByteString
  } deriving (Show, Generic, Binary, Serialize)


data GIFApplicationExtension = GIFApplicationExtension
  { applicationIdentifier :: ByteString
  , authenticationCode :: ByteString
  , applicationData :: ByteString
  } deriving (Show, Generic, Binary, Serialize)


data GIFExtension =
    GIFGraphicControlExtension GIFGraphicControlExtension
  | GIFCommentExtension GIFCommentExtension
  | GIFPlainTextExtension GIFPlainTextExtension
  | GIFApplicationExtension GIFApplicationExtension
  deriving (Show, Generic, Binary, Serialize)


data GIFImage = GIFImage
  { imageDescriptor :: GIFImageDescriptor
  , localColorTable :: Maybe [Word8]
  , imageData :: ByteString
  } deriving (Show, Generic, Binary, Serialize)


data GIF = GIF
  { header :: GIFHeader
  , screenDescriptor :: GIFScreenDescriptor
  , globalColorTable :: Maybe [Word8]
  , extensions :: [GIFExtension]
  , images :: [GIFImage]
  } deriving (Show, Generic, Binary, Serialize)


main :: IO ()
main = do
  let gif = GIF { header = GIFHeader "87a" 100 100 0 0 0
                , screenDescriptor = GIFScreenDescriptor 100 100 0 0 0
                , globalColorTable = Nothing
                , extensions = []
                , images = []
                }
  let bs = runPut (put gif)
  putStrLn $ "GIF size: " ++ show (BS.length bs)
  BS.writeFile "test.gif" bs


