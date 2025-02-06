bitcoin_transaction = {
  version: uint32le,
  input_count: varint,
  inputs: Input[input_count],
  output_count: varint,
  outputs: Output[output_count],
  lock_time: uint32le
};

Input = {
  previous_tx_hash: bytes(32),
  previous_output_index: uint32le,
  script_length: varint,
  script_sig: bytes(script_length),
  sequence: uint32le
};

Output = {
  value: uint64le,
  script_length: varint,
  script_pubkey: bytes(script_length)
};

varint = {
  first_byte: uint8,
  case first_byte < 0xFD {
    value: uint8
  },
  case first_byte == 0xFD {
    value: uint16le
  },
  case first_byte == 0xFE {
    value: uint32le
  },
  case first_byte == 0xFF {
    value: uint64le
  }
};