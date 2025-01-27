module GZIP.GzipGemini15 (..) where

import Daedalus.Type.AST

data GzipGemini15 = GzipGemini15 {
  field1 :: {a :: Integer, b :: String}
}

instance Daedalus GzipGemini15 where
  {-# INLINE daedalus #-}
  daedalus = do
    a <- integer
    b <- bytes
    return (GzipGemini15 {field1 = {a = a, b = b}})

main :: Daedalus ()
main = do
  x <- GzipGemini15
  return ()
