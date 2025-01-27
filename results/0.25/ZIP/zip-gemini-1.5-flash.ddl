module ZipGemini where

import Daedalus.TH

{-@ type ZipGemini = { zip :: { version :: Integer, files :: [File] } @-}

{-@ type File = { name :: String, data :: [Byte] } @-}

{-@ type Byte = Integer @-}

zipGemini :: Daedalus ZipGemini
zipGemini = do
  version <- integer
  files <- many file
  return $ ZipGemini { zip = ZipGemini' { version = version, files = files } }

file :: Daedalus File
file = do
  name <- string
  data <- many byte
  return $ File { name = name, data = data }

byte :: Daedalus Byte
byte = integer

integer :: Daedalus Integer
integer = be integer

string :: Daedalus String
string = utf8

instance Show ZipGemini where
  show (ZipGemini { zip = ZipGemini' { version, files } }) =
    "ZipGemini { version = " ++ show version ++ ", files = " ++ show files ++ " }"

instance Show File where
  show (File { name, data }) =
    "File { name = \"" ++ name ++ "\", data = " ++ show data ++ " }"

instance Show Byte where
  show = show

{-@ type ZipGemini' = { version :: Integer, files :: [File] } @-}
