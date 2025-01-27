meta:
  id: bitcoin_transaction
  file-extension: bitcoin-tx
  endian: le
  license: CC0-1.0
  ks-version: 0.9

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
        type: bytes
        size: 32
      - id: prev_tx_out_index
        type: u4
      - id: script_length
        type: vlq_base128_le
      - id: script_sig
        type: bytes
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
        type: bytes
        size: script_length

  vlq_base128_le:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: _io.pos >= _io.size or _.value & 0x80 == 0
    instances:
      result:
        value: >
          (_.value[0] & 0x7f) |
          ((_.value[1] & 0x7f) << 7) |
          ((_.value[2] & 0x7f) << 14) |
          ((_.value[3] & 0x7f) << 21) |
          ((_.value[4] & 0x7f) << 28) |
          ((_.value[5] & 0x7f) << 35) |
          ((_.value[6] & 0x7f) << 42) |
          ((_.value[7] & 0x7f) << 49) |
          ((_.value[8] & 0x7f) << 56) |
          ((_.value[9] & 0x7f) << 63)