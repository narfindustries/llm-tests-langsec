data ICMP = ICMP {
  type :: Word8,
  code :: Word8,
  checksum :: Word16,
  rest :: ByteString
}

instance Show ICMP where
  show (ICMP {type, code, checksum, rest}) =
    "ICMP { type: " <> show type <> ", code: " <> show code <> ", checksum: " <> show checksum <> ", rest: " <> show rest <> "}"

parseICMP :: Parser ICMP
parseICMP = do
  type <- leWord8
  code <- leWord8
  checksum <- leWord16
  rest <- getRemainingBytes
  return ICMP { type = type, code = code, checksum = checksum, rest = rest }
