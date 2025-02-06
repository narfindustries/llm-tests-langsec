def bitcoin : 
  type Transaction {
    version: uint32 little_endian,
    tx_in_count: varint,
    tx_in: TransactionInput[tx_in_count],
    tx_out_count: varint,
    tx_out: TransactionOutput[tx_out_count],
    lock_time: uint32 little_endian
  }

  type TransactionInput {
    previous_output: Hash32 big_endian,
    previous_index: uint32 little_endian,
    script_length: varint,
    script_sig: bytes script_length,
    sequence: uint32 little_endian
  }

  type TransactionOutput {
    value: uint64 little_endian,
    script_length: varint,
    script_pub_key: bytes script_length
  }

  type Hash32 {
    bytes: byte[32]
  }

  type varint {
    value: uint64,
    encoding: choice {
      case value < 0xfd: byte value
      case value <= 0xffff: byte 0xfd, uint16 little_endian value
      case value <= 0xffffffff: byte 0xfe, uint32 little_endian value
      case value <= 0xffffffffffffffff: byte 0xff, uint64 little_endian value
    }
  }

  type bytes {
    length: varint,
    value: byte[length]
  }

  type uint32 {
    value: uint32,
    little_endian: byte[4] value
  }

  type uint64 {
    value: uint64,
    little_endian: byte[8] value
  }

  type uint16 {
    value: uint16,
    little_endian: byte[2] value
  }