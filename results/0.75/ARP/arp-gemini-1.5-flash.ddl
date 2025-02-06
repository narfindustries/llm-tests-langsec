module ARP

import Data.Word
import Data.Vector (Vector, replicateM, (!))
import Data.Bits

data ARPHeader = ARPHeader {
  hardware_type :: Word16,
  protocol_type :: Word16,
  hardware_len :: Word8,
  protocol_len :: Word8,
  opcode :: Word16,
  sender_hardware_addr :: Vector Word8,
  sender_protocol_addr :: Vector Word8,
  target_hardware_addr :: Vector Word8,
  target_protocol_addr :: Vector Word8
}

instance Show ARPHeader where
  show (ARPHeader ht pt hl pl op sha spa tha tpa) =
    unwords ["ARPHeader",
             "hardware_type: " ++ show ht,
             "protocol_type: " ++ show pt,
             "hardware_len: " ++ show hl,
             "protocol_len: " ++ show pl,
             "opcode: " ++ show op,
             "sender_hardware_addr: " ++ show sha,
             "sender_protocol_addr: " ++ show spa,
             "target_hardware_addr: " ++ show tha,
             "target_protocol_addr: " ++ show tpa]

parseARPHeader :: Parser ARPHeader
parseARPHeader = do
  ht <- getWord16be
  pt <- getWord16be
  hl <- getWord8
  pl <- getWord8
  op <- getWord16be
  sha <- getVector hl
  spa <- getVector pl
  tha <- getVector hl
  tpa <- getVector pl
  return $ ARPHeader ht pt hl pl op sha spa tha tpa

getVector :: Int -> Parser (Vector Word8)
getVector n = do
  vec <- replicateM n getWord8
  return vec

