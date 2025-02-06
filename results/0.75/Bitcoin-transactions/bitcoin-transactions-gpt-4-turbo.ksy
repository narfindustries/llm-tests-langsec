meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  endian: le
  license: CC0-1.0
doc: |
  Structure of a Bitcoin transaction based on Bitcoin protocol
seq:
  - id: version
    type: u4
  - id: input_count
    type: vlq_base128_le
  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count.value
  - id: output_count
    type: vlq_base128_le
  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count.value
  - id: locktime
    type: u4
types:
  input:
    seq:
      - id: previous_output_hash
        type: u4
        repeat: expr
        repeat-expr: 8 # 32 bytes for the hash
      - id: previous_output_index
        type: u4
      - id: script_len
        type: vlq_base128_le
      - id: script_sig
        type: u1
        repeat: expr
        repeat-expr: script_len.value
      - id: sequence
        type: u4
  output:
    seq:
      - id: value
        type: u8
      - id: script_len
        type: vlq_base128_le
      - id: script_pub_key
        type: u1
        repeat: expr
        repeat-expr: script_len.value
  vlq_base128_le:
    seq:
      - id: bytes
        type: u1
        repeat: until
        repeat-until: _.last & 0x80 == 0
    instances:
      value:
        value: |
          _.reduce(0, (acc, byte) => (acc << 7) | (byte & 0x7F))