module HL7v2.Segments.MSH (
  MSH
) where

import Daedalus.Type.AST
import Daedalus.Value

data MSH = MSH
  { field1 :: {-# UNPACK #-} ByteString -- Field Separator
  , field2 :: {-# UNPACK #-} ByteString -- Encoding Characters
  , field3 :: {-# UNPACK #-} ByteString -- Sending Application
  , field4 :: {-# UNPACK #-} ByteString -- Sending Facility
  , field5 :: {-# UNPACK #-} ByteString -- Receiving Application
  , field6 :: {-# UNPACK #-} ByteString -- Receiving Facility
  , field7 :: {-# UNPACK #-} ByteString -- DateTime
  , field8 :: {-# UNPACK #-} ByteString -- Security
  , field9 :: {-# UNPACK #-} ByteString -- Message Type
  , field10 :: {-# UNPACK #-} ByteString -- Message Control ID
  , field11 :: {-# UNPACK #-} ByteString -- Processing ID
  , field12 :: {-# UNPACK #-} ByteString -- Version ID
  , field13 :: {-# UNPACK #-} ByteString -- Sequence Number
  , field14 :: {-# UNPACK #-} ByteString -- Continuation Pointer
  , field15 :: {-# UNPACK #-} ByteString -- Accept Acknowledgement Type
  , field16 :: {-# UNPACK #-} ByteString -- Application Acknowledgement Type
  , field17 :: {-# UNPACK #-} ByteString -- Country Code
  , field18 :: {-# UNPACK #-} ByteString -- Character Set
  , field19 :: {-# UNPACK #-} ByteString -- Principal Language of Message
  }

instance Show MSH where
  show (MSH f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19) =
    unwords [f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19]

parseMSH :: Parser MSH
parseMSH = do
  f1 <- some (satisfy isPrint)
  f2 <- some (satisfy isPrint)
  f3 <- some (satisfy isPrint)
  f4 <- some (satisfy isPrint)
  f5 <- some (satisfy isPrint)
  f6 <- some (satisfy isPrint)
  f7 <- some (satisfy isPrint)
  f8 <- some (satisfy isPrint)
  f9 <- some (satisfy isPrint)
  f10 <- some (satisfy isPrint)
  f11 <- some (satisfy isPrint)
  f12 <- some (satisfy isPrint)
  f13 <- some (satisfy isPrint)
  f14 <- some (satisfy isPrint)
  f15 <- some (satisfy isPrint)
  f16 <- some (satisfy isPrint)
  f17 <- some (satisfy isPrint)
  f18 <- some (satisfy isPrint)
  f19 <- some (satisfy isPrint)
  return $ MSH f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19

isPrint :: Char -> Bool
isPrint c = c >= ' ' && c <= '~'

