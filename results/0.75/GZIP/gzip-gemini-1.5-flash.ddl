module GZIP

import Daedalus.TypeRep

data Gzip = Gzip { id1 :: Word8,
                   id2 :: Word8,
                   cm :: Word8,
                   flg :: Word8,
                   mtime :: Word32,
                   xfl :: Word8,
                   os :: Word8,
                   extra :: Maybe (Array Word8),
                   fname :: Maybe (Array Word8),
                   comment :: Maybe (Array Word8),
                   headerCRC :: Maybe Word16,
                   compressedData :: Array Word8,
                   crc32 :: Word32,
                   isize :: Word32 }

instance Daedalus.TypeRep.TypeRep Gzip where
  typeRep _ = do
    id1 <- return 31
    id2 <- return 139
    cm  <- return 8
    flg <- bits 8
    mtime <- bits 32
    xfl <- bits 8
    os  <- bits 8
    extraLen <- if testBit flg 2 then Just <$> bits 16 else return Nothing
    extra <- maybe (return []) (\len -> count len (bits 8)) extraLen
    fname <- if testBit flg 3 then Just <$> manyTill (bits 8) (== 0) else return Nothing
    comment <- if testBit flg 4 then Just <$> manyTill (bits 8) (== 0) else return Nothing
    headerCRC <- if testBit flg 1 then Just <$> bits 16 else return Nothing
    compressedData <- many (bits 8)
    crc32 <- bits 32
    isize <- bits 32
    return (Gzip { id1 = id1, id2 = id2, cm = cm, flg = flg, mtime = mtime, xfl = xfl, os = os, extra = extra, fname = fname, comment = comment, headerCRC = headerCRC, compressedData = compressedData, crc32 = crc32, isize = isize})

testBit :: Word8 -> Bool
testBit n = (n .&. 1) /= 0

(.&.) :: Word8 -> Word8 -> Word8
(.&.) = undefined
