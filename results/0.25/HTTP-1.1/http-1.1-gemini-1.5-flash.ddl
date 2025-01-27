module Main where

import Daedalus.AST
import Daedalus.PP
import Daedalus.Compile

main :: IO ()
main = do
  let prog = do
        x <- bytes 8
        return x

  let compiledProg = compile prog
  case compiledProg of
    Left err -> putStrLn $ "Compilation failed: " ++ show err
    Right (compiled,_) -> do
      putStrLn "Compilation successful!"
      putStrLn $ "Compiled code: " ++ show compiled
