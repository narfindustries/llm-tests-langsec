module HL7v2.Segments.MSH (MSH) where

import Daedalus.Type.AST

data MSH = MSH
  { field1 :: {-# UNPACK #-} Maybe String
  , field2 :: {-# UNPACK #-} Maybe String
  , field3 :: {-# UNPACK #-} Maybe String
  , field4 :: {-# UNPACK #-} Maybe String
  , field5 :: {-# UNPACK #-} Maybe String
  , field6 :: {-# UNPACK #-} Maybe String
  , field7 :: {-# UNPACK #-} Maybe String
  , field8 :: {-# UNPACK #-} Maybe String
  , field9 :: {-# UNPACK #-} Maybe String
  , field10 :: {-# UNPACK #-} Maybe String
  }

instance Show MSH where
  show (MSH f1 f2 f3 f4 f5 f6 f7 f8 f9 f10) =
    concat
      [ "MSH|"
      , maybe "" (\x -> x ++ "|") f1
      , maybe "" (\x -> x ++ "|") f2
      , maybe "" (\x -> x ++ "|") f3
      , maybe "" (\x -> x ++ "|") f4
      , maybe "" (\x -> x ++ "|") f5
      , maybe "" (\x -> x ++ "|") f6
      , maybe "" (\x -> x ++ "|") f7
      , maybe "" (\x -> x ++ "|") f8
      , maybe "" (\x -> x ++ "|") f9
      , maybe "" (\x -> x ++ "|") f10
      ]

parseMSH :: Parser MSH
parseMSH = do
  _ <- symbol "MSH"
  f1 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f2 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f3 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f4 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f5 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f6 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f7 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f8 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f9 <- optional (some (satisfy isNotPipe))
  _ <- optional (symbol "|")
  f10 <- optional (some (satisfy isNotPipe))
  return $ MSH f1 f2 f3 f4 f5 f6 f7 f8 f9 f10

isNotPipe :: Char -> Bool
isNotPipe c = c /= '|'


main :: IO ()
main = do
  let input = "MSH|^~\\&||||||202310271000||"
  case runParser parseMSH input of
    Just msh -> print (show msh)
    Nothing -> putStrLn "Parse failed"

