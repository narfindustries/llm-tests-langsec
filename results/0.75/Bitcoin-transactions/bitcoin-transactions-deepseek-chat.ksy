meta:
  id: bitcoin_transaction
  file-extension: btc
  endian: le
  imports:
    - bitcoin_common

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
      - id: previous_output
        type: outpoint
      - id: script_length
        type: vlq_base128_le
      - id: signature_script
        size: script_length
      - id: sequence
        type: u4

  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: vlq_base128_le
      - id: pk_script
        size: script_length

  outpoint:
    seq:
      - id: hash
        size: 32
      - id: index
        type: u4

  vlq_base128_le:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _io.pos >= _io.size || (_io.read_bits_int(1) == 0)