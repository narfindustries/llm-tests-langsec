module Main where

import Daedalus.AST
import Daedalus.PP

-- Sample Daedalus code (replace with your actual code)
main :: Daedalus.AST.Module
main = Module "Main" [] [
  Def (NoLoc "test") (tUnit `fun` \x -> x)
]

