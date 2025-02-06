BitcoinTransaction ::=
  struct {
    version: uint32,
    inputs: array<Input>,
    outputs: array<Output>,
    locktime: uint32
  }

Input ::=
  struct {
    prevTxHash: bytes[32],
    outputIndex: uint32,
    scriptSig: Script,
    sequence: uint32
  }

Output ::=
  struct {
    value: uint64,
    scriptPubKey: Script
  }

Script ::=
  struct {
    length: VarInt,
    scriptBytes: bytes[length]
  }

VarInt ::=
  choice {
    as u8: uint8 when u8 < 0xFD,
    as u16: uint16 when u8 == 0xFD,
    as u32: uint32 when u8 == 0xFE,
    as u64: uint64 when u8 == 0xFF
  }