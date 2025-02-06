format binary

type Transaction {
  version: uint32 little_endian,
  tx_in_count: varint,
  tx_in: TransactionInput[tx_in_count],
  tx_out_count: varint,
  tx_out: TransactionOutput[tx_out_count],
  lock_time: uint32 little_endian
}

type TransactionInput {
  previous_output: bytes(36),
  script_length: varint,
  scriptSig: bytes[script_length],
  sequence: uint32 little_endian
}

type TransactionOutput {
  value: uint64 little_endian,
  script_length: varint,
  scriptPubKey: bytes[script_length]
}

type Block {
  block_header: BlockHeader,
  transaction_count: varint,
  transactions: Transaction[transaction_count]
}

type BlockHeader {
  version: uint32 little_endian,
  previous_block_hash: bytes(32),
  merkle_root: bytes(32),
  timestamp: uint32 little_endian,
  target: uint32 little_endian,
  nonce: uint32 little_endian
}

type NetworkMessage {
  magic: bytes(4),
  command: bytes(12),
  payload_length: varint,
  payload: bytes[payload_length],
  checksum: bytes(4)
}

type VersionMessage {
  version: uint32 little_endian,
  services: uint64 little_endian,
  timestamp: uint64 little_endian,
  addr_recv: NetAddress,
  addr_from: NetAddress,
  nonce: uint64 little_endian,
  user_agent: bytes[varint],
  start_height: uint32 little_endian,
  relay: bool
}

type NetAddress {
  time: uint32 little_endian,
  services: uint64 little_endian,
  address: bytes(16),
  port: uint16 big_endian
}

type VarInt {
  value: uint64
}

function varint(value: uint64): bytes {
  if value < 0xfd {
    return bytes(1) + value
  } else if value <= 0xffff {
    return bytes(1) + 0xfd + uint16 little_endian(value)
  } else if value <= 0xffffffff {
    return bytes(1) + 0xfe + uint32 little_endian(value)
  } else {
    return bytes(1) + 0xff + uint64 little_endian(value)
  }
}