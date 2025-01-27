module Main where

import Daedalus.AST
import Daedalus.PP
import Daedalus.Compile

main :: IO ()
main = do
  let prog = do
        -- Your Daedalus code here.  The error message doesn't provide enough
        -- information to determine what the original code was, so this is a
        -- placeholder.  Replace this with your actual code.

        -- Example:  A simple parser for a number followed by a newline.
        num <- decimal
        newline
        return num

  case compile prog of
    Left err -> putStrLn $ "Compilation failed: " ++ show err
    Right compiled -> do
      putStrLn "Compilation successful!"
      --  Further processing of the compiled code could go here.  For example,
      --  writing it to a file.
