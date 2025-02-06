module ARP {
  import Data.Word;
  import Data.Array;

  data ARPHeader = ARPHeader {
    hardwareType :: Word16,
    protocolType :: Word16,
    hardwareAddressLength :: Word8,
    protocolAddressLength :: Word8,
    operation :: Word16,
    senderHardwareAddress :: Array Word8 6,
    senderProtocolAddress :: Array Word8 4,
    targetHardwareAddress :: Array Word8 6,
    targetProtocolAddress :: Array Word8 4
  } deriving (Show, Eq)

  parseARPHeader :: Parser ARPHeader
  parseARPHeader = do
    hardwareType <- beWord16
    protocolType <- beWord16
    hardwareAddressLength <- word8
    protocolAddressLength <- word8
    operation <- beWord16
    senderHardwareAddress <- count 6 word8
    senderProtocolAddress <- count 4 word8
    targetHardwareAddress <- count 6 word8
    targetProtocolAddress <- count 4 word8
    return ARPHeader {
      hardwareType = hardwareType,
      protocolType = protocolType,
      hardwareAddressLength = hardwareAddressLength,
      protocolAddressLength = protocolAddressLength,
      operation = operation,
      senderHardwareAddress = senderHardwareAddress,
      senderProtocolAddress = senderProtocolAddress,
      targetHardwareAddress = targetHardwareAddress,
      targetProtocolAddress = targetProtocolAddress
    }

  generateARPHeader :: ARPHeader -> [Word8]
  generateARPHeader (ARPHeader htype ptype hlen plen op sha spa tha tpa) =
    concat [
      encodeWord16be htype,
      encodeWord16be ptype,
      encodeWord8 hlen,
      encodeWord8 plen,
      encodeWord16be op,
      sha,
      spa,
      tha,
      tpa
    ]

  encodeWord16be :: Word16 -> [Word8]
  encodeWord16be w = [fromIntegral (w `shiftR` 8), fromIntegral (w .&. 0xFF)]

  encodeWord8 :: Word8 -> [Word8]
  encodeWord8 w = [w]

}
