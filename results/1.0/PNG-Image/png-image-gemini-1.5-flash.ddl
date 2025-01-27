{-# LANGUAGE OverloadedStrings #-}

module PNG-Image.png-image-gemini-1 where

import Daedalus.Panic
import Daedalus.Type.AST

import Daedalus.Value
import Daedalus.PP

data PNG = PNG {
    width :: Integer,
    height :: Integer,
    pixels :: [Pixel]
} deriving (Show,Eq)

data Pixel = Pixel {
    r :: Integer,
    g :: Integer,
    b :: Integer
} deriving (Show,Eq)


png :: Daedalus.Type.AST.Value -> Daedalus.Type.AST.Value -> Daedalus.Type.AST.Value -> Daedalus.Type.AST.Value
png width height pixels =
    let
        w = case width of
            Just w' -> w'
            Nothing -> panic "width not found"
        h = case height of
            Just h' -> h'
            Nothing -> panic "height not found"
        p = case pixels of
            Just p' -> p'
            Nothing -> panic "pixels not found"
    in
        PNG w h p


parsePNG :: Daedalus.Type.AST.Value -> Daedalus.Type.AST.Value -> Daedalus.Type.AST.Value -> Daedalus.Type.AST.Value
parsePNG width height pixels =
  do
    w <- fromInteger width
    h <- fromInteger height
    ps <- fromList pixels
    return (PNG w h ps)


instance Daedalus.Type.AST.Semigroup PNG where
  (<>) (PNG w1 h1 p1) (PNG w2 h2 p2) = PNG (w1 + w2) (h1 + h2) (p1 ++ p2)

instance Daedalus.Type.AST.Monoid PNG where
  mempty = PNG 0 0 []

instance Daedalus.Type.AST.Show PNG where
    showsPrec p (PNG w h ps) = showParen (p > 10) $
        showString "PNG {" .
        showString "width=" . showsPrec 11 w .
        showString ", height=" . showsPrec 11 h .
        showString ", pixels=" . showsPrec 11 ps .
        showString "}"

{-
main :: IO ()
main = do
    let result = parsePNG (Just 100) (Just 100) (Just [Pixel 255 0 0, Pixel 0 255 0])
    print result
-}
