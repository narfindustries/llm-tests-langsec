data TIFF = TIFF {
  byteOrder :: ByteOrder,
  ifd :: [IFD]
}

data ByteOrder = LittleEndian | BigEndian

data IFD = IFD {
  entries :: [IFDEntry],
  nextIFDOffset :: Maybe Word32
}

data IFDEntry = IFDEntry {
  tag :: Word16,
  fieldType :: FieldType,
  count :: Word32,
  valueOffset :: Word32
}

data FieldType = FT_BYTE | FT_ASCII | FT_SHORT | FT_LONG | FT_RATIONAL | FT_SBYTE | FT_SSHORT | FT_SLONG | FT_SRATIONAL | FT_FLOAT | FT_DOUBLE | FT_UNDEFINED
  deriving (Show, Eq)

parseWord16 :: ByteOrder -> [Word8] -> Maybe (Word16, [Word8])
parseWord16 LittleEndian (a:b:rest) = Just ((fromIntegral a) + ((fromIntegral b) `shiftL` 8), rest)
parseWord16 BigEndian (a:b:rest) = Just ((fromIntegral b) + ((fromIntegral a) `shiftL` 8), rest)
parseWord16 _ _ = Nothing

parseWord32 :: ByteOrder -> [Word8] -> Maybe (Word32, [Word8])
parseWord32 LittleEndian (a:b:c:d:rest) = Just ((fromIntegral a) + ((fromIntegral b) `shiftL` 8) + ((fromIntegral c) `shiftL` 16) + ((fromIntegral d) `shiftL` 24), rest)
parseWord32 BigEndian (a:b:c:d:rest) = Just ((fromIntegral d) + ((fromIntegral c) `shiftL` 8) + ((fromIntegral b) `shiftL` 16) + ((fromIntegral a) `shiftL` 24), rest)
parseWord32 _ _ = Nothing

parseBytes :: Word32 -> [Word8] -> Maybe ([Word8], [Word8])
parseBytes n xs = case splitAt (fromIntegral n) xs of
  (ys, zs) | length ys == fromIntegral n -> Just (ys, zs)
  _ -> Nothing

parseTIFF :: [Word8] -> Maybe TIFF
parseTIFF bytes = do
  (byteOrder', rest) <- parseByteOrder bytes
  (ifdOffset, rest') <- parseWord32 byteOrder' rest
  let ifdOffset' = fromIntegral ifdOffset
  (ifd, _) <- parseIFD byteOrder' rest' ifdOffset'
  return $ TIFF byteOrder' ifd

parseByteOrder :: [Word8] -> Maybe (ByteOrder, [Word8])
parseByteOrder (a:b:rest) = case (a,b) of
  (0x49, 0x49) -> Just (LittleEndian, rest)
  (0x4D, 0x4D) -> Just (BigEndian, rest)
  _ -> Nothing
parseByteOrder _ = Nothing

parseIFD :: ByteOrder -> [Word8] -> Word32 -> Maybe ([IFD], [Word8])
parseIFD byteOrder bytes offset = do
  (numEntries, rest) <- parseWord16 byteOrder bytes
  let numEntries' = fromIntegral numEntries
  entries <- traverse (\i -> parseIFDEntry byteOrder rest offset) [1..numEntries']
  (nextOffset, rest') <- parseWord32 byteOrder (snd $ last entries)
  return ([IFD entries (if nextOffset == 0 then Nothing else Just nextOffset)], rest')

parseIFDEntry :: ByteOrder -> [Word8] -> Word32 -> Int -> Maybe (IFDEntry, [Word8])
parseIFDEntry byteOrder bytes offset i = do
  (tag, rest) <- parseWord16 byteOrder bytes
  (fieldType, rest') <- parseWord16 byteOrder rest
  (count, rest'') <- parseWord32 byteOrder rest'
  return (IFDEntry tag fieldType count (offset + 12 + (fromIntegral i * 12)), rest'')

