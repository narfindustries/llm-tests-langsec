{-# LANGUAGE OverloadedStrings #-}
module ZipGemini where

import Daedalus.AST
import Daedalus.PP

data ZipGemini = ZipGemini {
  version :: {-# UNPACK #-} !Word8,
  data :: {-# UNPACK #-} !ByteString
} deriving (Show, Eq)

zipGeminiCodec :: Daedalus.AST.Codec ZipGemini
zipGeminiCodec = do
  version' <- word8
  data' <- bytes
  return $ ZipGemini version' data'

instance Semigroup ZipGemini where
  ZipGemini v1 d1 <> ZipGemini v2 d2 = ZipGemini v1 (d1 <> d2)

instance Monoid ZipGemini where
  mempty = ZipGemini 0 ""

main :: IO ()
main = do
  let codec = zipGeminiCodec
  putStrLn $ show $ pp codec
