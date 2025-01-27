module Main where

import Net.IPv4
import Data.Bytes
import Data.Bytes.Combinators
import Data.Word

-- Basic types
type Word8 = U8
type Word16 = U16be

-- Main ICMP packet type
data ICMPType =
    EchoReply |
    DestinationUnreachable |
    SourceQuench |
    Redirect |
    Echo |
    TimeExceeded |
    ParameterProblem |
    Timestamp |
    TimestampReply |
    InfoRequest |
    InfoReply |
    Other Word8

-- ICMP messages vary based on type
data ICMPMessage =
    Unreachable UnreachableMessage |
    TimeExceededMsg TimeExceededMessage |
    EchoMsg EchoMessage

-- Unreachable message format
data UnreachableMessage = UnreachableMessage
  { unused :: Word16
  , nextHopMTU :: Word16
  , ipHeader :: Bytes 20
  , first8bytesOfOriginalDatagram :: Bytes 8
  }

-- Time exceeded message format
data TimeExceededMessage = TimeExceededMessage
  { unused :: Word16
  , unused2 :: Word16
  , ipHeader :: Bytes 20
  , first8bytesOfOriginalDatagram :: Bytes 8
  }

-- Echo (Request/Reply) message format
data EchoMessage = EchoMessage
  { identifier :: Word16
  , sequenceNumber :: Word16
  , dataField :: Bytes
  }

-- To parse the type of ICMP message
parseICMPType :: Word8 -> ICMPType
parseICMPType 0 = EchoReply
parseICMPType 3 = DestinationUnreachable
parseICMPType 4 = SourceQuench
parseICMPType 5 = Redirect
parseICMPType 8 = Echo
parseICMPType 11 = TimeExceeded
parseICMPType 12 = ParameterProblem
parseICMPType 13 = Timestamp
parseICMPType 14 = TimestampReply
parseICMPType 15 = InfoRequest
parseICMPType 16 = InfoReply
parseICMPType t = Other t

-- Parsing depending on type
parseICMPMessage :: ICMPType -> Parser ICMPMessage
parseICMPMessage DestinationUnreachable = Unreachable <$> parseUnreachable
parseICMPMessage TimeExceeded = TimeExceededMsg <$> parseTimeExceeded
parseICMPMessage Echo = EchoMsg <$> parseEcho
parseICMPMessage _ = fail "Unsupported ICMP type for detailed parsing"

-- Parsing individual message types
parseUnreachable :: Parser UnreachableMessage
parseUnreachable = UnreachableMessage <$> word16 <*> word16 <*> bytes 20 <*> bytes 8

parseTimeExceeded :: Parser TimeExceededMessage
parseTimeExceeded = TimeExceededMessage <$> word16 <*> word16 <*> bytes 20 <*> bytes 8

parseEcho :: Parser EchoMessage
parseEcho = EchoMessage <$> word16 <*> word16 <*> many anyByte

-- Main parsing function
parseICMP :: Parser (ICMPType, ICMPMessage)
parseICMP = do
  t <- anyByte
  let typ = parseICMPType t
  msg <- parseICMPMessage typ
  return (typ, msg)