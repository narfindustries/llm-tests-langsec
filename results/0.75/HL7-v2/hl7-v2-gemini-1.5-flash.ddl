module HL7v2.Segments.MSH (
  MSH
) where

import Daedalus.TH
import Daedalus.Type.AST

data MSH = MSH
  { field1 :: String
  , field2 :: String
  , field3 :: String
  , field4 :: String
  , field5 :: String
  , field6 :: String
  , field7 :: String
  , field8 :: String
  , field9 :: String
  , field10 :: String
  } deriving (Show, Eq)


instance Daedalus MSH where
  daedalus = do
    field1 <- some (char8) `sepBy` char8 ','
    field2 <- some (char8) `sepBy` char8 ','
    field3 <- some (char8) `sepBy` char8 ','
    field4 <- some (char8) `sepBy` char8 ','
    field5 <- some (char8) `sepBy` char8 ','
    field6 <- some (char8) `sepBy` char8 ','
    field7 <- some (char8) `sepBy` char8 ','
    field8 <- some (char8) `sepBy` char8 ','
    field9 <- some (char8) `sepBy` char8 ','
    field10 <- some (char8) `sepBy` char8 ','
    return MSH {field1, field2, field3, field4, field5, field6, field7, field8, field9, field10}

