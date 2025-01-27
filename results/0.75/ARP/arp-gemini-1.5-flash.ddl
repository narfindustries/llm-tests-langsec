module Main where

import Daedalus.TH

{-# LANGUAGE OverloadedStrings #-}

data MyRecord = MyRecord { field1 :: String, field2 :: Int }

mkRecord :: String -> Int -> MyRecord
mkRecord a b = MyRecord { field1 = a, field2 = b }

main :: Daedalus.TH.Program
main = do
  let record1 = mkRecord "hello" 10
      record2 = mkRecord "world" 20
  return $ do
    putStrLn (show record1)
    putStrLn (show record2)

