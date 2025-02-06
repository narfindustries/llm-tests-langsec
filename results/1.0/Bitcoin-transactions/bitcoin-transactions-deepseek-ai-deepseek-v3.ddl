bitcoin_transaction: struct {
  version: uint32le,
  inputs: array[
    struct {
      prev_tx_hash: bytes(32),
      prev_output_index: uint32le,
      script_length: varint,
      unlocking_script: bytes(script_length),
      sequence: uint32le,
    },
    length: varint,
  ],
  outputs: array[
    struct {
      amount: uint64le,
      script_length: varint,
      locking_script: bytes(script_length),
    },
    length: varint,
  ],
  lock_time: uint32le,
};