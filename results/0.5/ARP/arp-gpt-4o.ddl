module ARP

import Data.Word
import Data.ByteString

data ARP = ARP
  { htype     :: Word16  -- Hardware type
  , ptype     :: Word16  -- Protocol type
  , hlen      :: Word8   -- Hardware address length
  , plen      :: Word8   -- Protocol address length
  , oper      :: Word16  -- Operation
  , sha       :: ByteString  -- Sender hardware address
  , spa       :: ByteString  -- Sender protocol address
  , tha       :: ByteString  -- Target hardware address
  , tpa       :: ByteString  -- Target protocol address
  }
  deriving (Show, Eq)

parseARP :: ByteString -> Maybe ARP
parseARP bs = do
  guard (BS.length bs >= 28)
  let htype = runGet getWord16be bs
      ptype = runGet getWord16be (BS.drop 2 bs)
      hlen  = runGet getWord8 (BS.drop 4 bs)
      plen  = runGet getWord8 (BS.drop 5 bs)
      oper  = runGet getWord16be (BS.drop 6 bs)
      sha   = BS.take 6 (BS.drop 8 bs)
      spa   = BS.take 4 (BS.drop 14 bs)
      tha   = BS.take 6 (BS.drop 18 bs)
      tpa   = BS.take 4 (BS.drop 24 bs)
  return ARP {..}

serializeARP :: ARP -> ByteString
serializeARP arp = runPut $ do
  putWord16be (htype arp)
  putWord16be (ptype arp)
  putWord8 (hlen arp)
  putWord8 (plen arp)
  putWord16be (oper arp)
  putByteString (sha arp)
  putByteString (spa arp)
  putByteString (tha arp)
  putByteString (tpa arp)