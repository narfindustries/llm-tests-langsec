BitcoinTransaction = struct {
  version : uint32,
  inputs : array<Input>(varint),
  outputs : array<Output>(varint),
  locktime : uint32
};

Input = struct {
  prevTxHash : bytes[32],
  outputIndex : uint32,
  scriptSig : bytes(varint),
  sequence : uint32
};

Output = struct {
  value : uint64,
  scriptPubKey : bytes(varint)
};

varint = union {
  uint8 : uint8 when value < 0xFD,
  uint16 : uint16 when value == 0xFD,
  uint32 : uint32 when value == 0xFE,
  uint64 : uint64 when value == 0xFF
};