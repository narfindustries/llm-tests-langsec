meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  license: CC0-1.0
  endian: le
seq:
  - id: version
    type: u4
  - id: in_count
    type: vlq_base128_le
  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: in_count
  - id: out_count
    type: vlq_base128_le
  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: out_count
  - id: lock_time
    type: u4
types:
  tx_input:
    seq:
      - id: prev_tx_hash
        size: 32
      - id: prev_tx_out_index
        type: u4
      - id: script_length
        type: vlq_base128_le
      - id: script_sig
        size: script_length
      - id: sequence
        type: u4
  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: vlq_base128_le
      - id: script_pubkey
        size: script_length
  vlq_base128_le:
    seq:
      - id: value
        type: b1
        repeat: until
        repeat-until: _io.eof?