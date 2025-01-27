{-# LANGUAGE OverloadedStrings #-}
module ZipGemini where

import Daedalus.AST
import Daedalus.Type

data ZipGemini = ZipGemini {
  header :: {-# UNPACK #-} !(Maybe ByteString),
  entries :: [Entry]
}

data Entry = Entry {
  name :: {-# UNPACK #-} !ByteString,
  size :: {-# UNPACK #-} !Word64,
  data :: {-# UNPACK #-} !ByteString
}

instance Semigroup ZipGemini where
  ZipGemini h1 e1 <> ZipGemini h2 e2 = ZipGemini (h1 <|> h2) (e1 ++ e2)

instance Monoid ZipGemini where
  mempty = ZipGemini Nothing []

zipGeminiParser :: Parser ZipGemini
zipGeminiParser = do
  header <- optional (some byte)
  entries <- many entryParser
  return $ ZipGemini (Just header) entries

entryParser :: Parser Entry
entryParser = do
  name <- some byte
  size <- beWord64be
  data <- count size byte
  return $ Entry name size data

--Example usage (replace with your actual data)
main :: IO ()
main = do
  let exampleZip = ZipGemini (Just "Header Data" ) [Entry "file1.txt" 10 "file1content" , Entry "file2.txt" 5 "file2content"]
  let encoded = encode exampleZip
  putStrLn $ show encoded

--Helper functions for encoding and decoding.  These are just examples and might need adjustments based on your specific needs.
encode :: ZipGemini -> ByteString
encode (ZipGemini h entries) = mconcat [ maybe "" id h, mconcat $ map encodeEntry entries]

encodeEntry :: Entry -> ByteString
encodeEntry (Entry n s d) = n <> encodeWord64be s <> d

encodeWord64be :: Word64 -> ByteString
encodeWord64be w = packBE w

packBE :: Word64 -> ByteString
packBE w = B.pack [fromIntegral (w `shiftR` 56), fromIntegral (w `shiftR` 48), fromIntegral (w `shiftR` 40), fromIntegral (w `shiftR` 32), fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 8), fromIntegral w]


beWord64be :: Parser Word64
beWord64be = do
  bs <- count 8 byte
  return $ fromIntegral (B.unpack bs !! 0) `shiftL` 56
       + fromIntegral (B.unpack bs !! 1) `shiftL` 48
       + fromIntegral (B.unpack bs !! 2) `shiftL` 40
       + fromIntegral (B.unpack bs !! 3) `shiftL` 32
       + fromIntegral (B.unpack bs !! 4) `shiftL` 24
       + fromIntegral (B.unpack bs !! 5) `shiftL` 16
       + fromIntegral (B.unpack bs !! 6) `shiftL` 8
       + fromIntegral (B.unpack bs !! 7)

import qualified Data.ByteString as B
import Data.ByteString (ByteString, pack)
import Data.Word (Word64)
import Data.Maybe (maybe)
