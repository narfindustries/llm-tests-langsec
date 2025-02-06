module ARP {
  import Data.Word
  import Data.Bits

  data ARPPacket = ARPPacket {
    hardwareType :: Word16,
    protocolType :: Word16,
    hardwareAddressLength :: Word8,
    protocolAddressLength :: Word8,
    opcode :: Word16,
    senderHardwareAddress :: [Word8],
    senderProtocolAddress :: [Word8],
    targetHardwareAddress :: [Word8],
    targetProtocolAddress :: [Word8]
  }

  parseARP :: Get ARPPacket
  parseARP = do
    htype <- getWord16be
    ptype <- getWord16be
    hlen <- getWord8
    plen <- getWord8
    opcode <- getWord16be
    sha <- getBytes hlen
    spa <- getBytes plen
    tha <- getBytes hlen
    tpa <- getBytes plen
    return $ ARPPacket htype ptype hlen plen opcode sha spa tha tpa

  showARP :: ARPPacket -> String
  showARP (ARPPacket htype ptype hlen plen opcode sha spa tha tpa) =
    unlines [
      "Hardware Type: " ++ show htype,
      "Protocol Type: " ++ show ptype,
      "Hardware Address Length: " ++ show hlen,
      "Protocol Address Length: " ++ show plen,
      "Opcode: " ++ show opcode,
      "Sender Hardware Address: " ++ show sha,
      "Sender Protocol Address: " ++ show spa,
      "Target Hardware Address: " ++ show tha,
      "Target Protocol Address: " ++ show tpa
    ]

  getBytes :: Int -> Get [Word8]
  getBytes n = replicateM n getWord8
}
