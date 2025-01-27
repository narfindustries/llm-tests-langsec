module GZIP.GzipGemini15Flash where

import Daedalus.Type.AST

data GzipGemini15Flash = GzipGemini15Flash {
  field1 :: { a :: Word8, b :: Word16 }
, field2 :: [Word8]
}

parse :: Parser GzipGemini15Flash
parse = do
  field1 <- parseField1
  field2 <- parseField2
  return GzipGemini15Flash { field1 = field1, field2 = field2 }

parseField1 :: Parser { a :: Word8, b :: Word16 }
parseField1 = do
  a <- word8
  b <- word16
  return { a = a, b = b }

parseField2 :: Parser [Word8]
parseField2 = many word8

-- Example usage (for testing):
main :: IO ()
main = do
  let exampleData = [0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
  case runParser parse exampleData of
    Just result -> print result
    Nothing -> putStrLn "Parsing failed"

