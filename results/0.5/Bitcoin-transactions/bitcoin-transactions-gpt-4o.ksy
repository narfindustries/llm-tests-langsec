meta:
  id: bitcoin_transactions
  file-extension: btc
  endian: little

seq:
  - id: version
    type: u4

  - id: input_count
    type: vlq_base128_le

  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count

  - id: output_count
    type: vlq_base128_le

  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count

  - id: lock_time
    type: u4

types:
  input:
    seq:
      - id: previous_output
        type: outpoint

      - id: script_length
        type: vlq_base128_le

      - id: script
        size: script_length

      - id: sequence
        type: u4

  output:
    seq:
      - id: value
        type: u8

      - id: script_length
        type: vlq_base128_le

      - id: script
        size: script_length

  outpoint:
    seq:
      - id: hash
        type: bytes
        size: 32

      - id: index
        type: u4

