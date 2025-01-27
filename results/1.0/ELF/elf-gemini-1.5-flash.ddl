module Main where

import Daedalus.Compiler
import Daedalus.Panic

main :: IO ()
main = do
  let prog =  do
          -- Example program.  Replace this with your actual program.
          pure ()
  compile prog "elf-gemini-1"
