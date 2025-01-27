module Main where

import Daedalus.Compiler
import Daedalus.Parser
import Daedalus.PP

main :: IO ()
main = do
  let source = "generated/999999/1.0/NITF/nitf-gemini-1.5-flash.ddl"
      outputDir = "generated/999999/1.0/NITF/output_daedalus/nitf-gemini-1"
  result <- compileDaedalus source outputDir
  case result of
    Left err -> print (pp err)
    Right () -> putStrLn "Compilation successful!"

compileDaedalus :: FilePath -> FilePath -> IO (Either CompileError ())
compileDaedalus source outputDir = do
  let cmd = ["/home/user/daedalus/DIR/daedalus", "compile-hs", source, "--out-dir", outputDir]
  exitCode <- runProcess "daedalus" cmd [] Nothing Nothing
  if exitCode == 0
    then return $ Right ()
    else return $ Left $ CompileError "Daedalus compilation failed"

data CompileError = CompileError String
  deriving (Show)

instance Exception CompileError
