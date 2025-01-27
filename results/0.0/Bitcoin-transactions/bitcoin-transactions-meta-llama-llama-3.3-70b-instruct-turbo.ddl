domain bitcoin_transactions {
  type transaction = struct {
    version: uint32,
    num_inputs: varint,
    inputs: array(transaction_input, num_inputs),
    num_outputs: varint,
    outputs: array(transaction_output, num_outputs),
    lock_time: uint32
  }

  type transaction_input = struct {
    prev_tx_hash: bytes(32),
    prev_tx_out_index: uint32,
    script_sig: bytes(varint),
    sequence: uint32
  }

  type transaction_output = struct {
    value: int64,
    script_pub_key: bytes(varint)
  }

  type varint = uint64 {
    encoding: varint
  }
}