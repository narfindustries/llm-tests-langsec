{-# LANGUAGE OverloadedStrings #-}
module Network.Time.Protocol.Version.4 where

import Daedalus.AST
import Daedalus.PP

data NTPVersion4 = NTPVersion4 {
  leapIndicator :: Bit 2,
  versionNumber :: Bit 3,
  mode :: Bit 3,
  stratum :: UInt8,
  poll :: Int8,
  precision :: Int8,
  rootDelay :: TimeDelta,
  rootDispersion :: TimeDelta,
  referenceID :: ReferenceID,
  referenceTimestamp :: Timestamp,
  originateTimestamp :: Timestamp,
  receiveTimestamp :: Timestamp,
  transmitTimestamp :: Timestamp
  }

data ReferenceID = ReferenceID {
  referenceId ::  Bytes 4
  }

data TimeDelta = TimeDelta {
  seconds :: UInt32,
  fraction :: UInt32
  }

data Timestamp = Timestamp {
  seconds :: UInt32,
  fraction :: UInt32
  }

instance PP NTPVersion4 where
  pp (NTPVersion4 li vn mo st p pr rd rdsp rid rt ot rt tmt) =
    "NTPVersion4" <+>
    "{" <+>
    "leapIndicator=" <+> pp li <+> "," <+>
    "versionNumber=" <+> pp vn <+> "," <+>
    "mode=" <+> pp mo <+> "," <+>
    "stratum=" <+> pp st <+> "," <+>
    "poll=" <+> pp p <+> "," <+>
    "precision=" <+> pp pr <+> "," <+>
    "rootDelay=" <+> pp rd <+> "," <+>
    "rootDispersion=" <+> pp rdsp <+> "," <+>
    "referenceID=" <+> pp rid <+> "," <+>
    "referenceTimestamp=" <+> pp rt <+> "," <+>
    "originateTimestamp=" <+> pp ot <+> "," <+>
    "receiveTimestamp=" <+> pp rt <+> "," <+>
    "transmitTimestamp=" <+> pp tmt <+>
    "}"

instance PP ReferenceID where
    pp (ReferenceID id) = "ReferenceID" <+> "{" <+> "referenceId=" <+> pp id <+> "}"

instance PP TimeDelta where
    pp (TimeDelta s f) = "TimeDelta" <+> "{" <+> "seconds=" <+> pp s <+> "," <+> "fraction=" <+> pp f <+> "}"

instance PP Timestamp where
    pp (Timestamp s f) = "Timestamp" <+> "{" <+> "seconds=" <+> pp s <+> "," <+> "fraction=" <+> pp f <+> "}"


ntpVersion4Parser :: Parser NTPVersion4
ntpVersion4Parser = do
  li <- bits 2
  vn <- bits 3
  mo <- bits 3
  st <- uint8
  p <- int8
  pr <- int8
  rd <- timeDeltaParser
  rdsp <- timeDeltaParser
  rid <- referenceIDParser
  rt <- timestampParser
  ot <- timestampParser
  rc <- timestampParser
  tmt <- timestampParser
  return $ NTPVersion4 li vn mo st p pr rd rdsp rid rt ot rc tmt

referenceIDParser :: Parser ReferenceID
referenceIDParser = ReferenceID <$> bytes 4

timeDeltaParser :: Parser TimeDelta
timeDeltaParser = do
  s <- uint32
  f <- uint32
  return $ TimeDelta s f

timestampParser :: Parser Timestamp
timestampParser = do
  s <- uint32
  f <- uint32
  return $ Timestamp s f

main :: IO ()
main = do
  let p = ntpVersion4Parser
  putStrLn $ show $ pp p
