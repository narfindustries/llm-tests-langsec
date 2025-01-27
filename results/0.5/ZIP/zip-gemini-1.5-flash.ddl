module ZipGemini1 where

import Daedalus.Type.AST

data ZipGemini1 = ZipGemini1 {
  field1 :: {a :: Integer, b :: String}
}

zipGemini1Parser :: Parser ZipGemini1
zipGemini1Parser = do
  a <- integer
  b <- string
  return $ ZipGemini1 { field1 = {a = a, b = b} }

zipGemini1Generator :: ZipGemini1 -> Gen ZipGemini1
zipGemini1Generator (ZipGemini1 {field1 = {a = a, b = b}}) = do
  return $ ZipGemini1 {field1 = {a = a, b = b}}

main :: IO ()
main = do
  let test = ZipGemini1 { field1 = {a = 123, b = "hello"} }
  print (zipGemini1Parser test)
  print (zipGemini1Generator test)
