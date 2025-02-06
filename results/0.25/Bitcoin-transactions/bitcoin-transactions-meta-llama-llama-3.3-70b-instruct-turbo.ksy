meta:
  id: bitcoin_transactions
  endian: le

seq:
  - id: version
    type: u4le
  - id: tx_in_count
    type: varint
  - id: tx_ins
    type: tx_in
    repeat: expr
    repeat-expr: tx_in_count
  - id: tx_out_count
    type: varint
  - id: tx_outs
    type: tx_out
    repeat: expr
    repeat-expr: tx_out_count
  - id: lock_time
    type: u4le

types:
  tx_in:
    seq:
      - id: prev_out_hash
        type: bytes
        size: 32
      - id: prev_out_index
        type: u4le
      - id: script_length
        type: varint
      - id: script
        type: script
        size: script_length
      - id: sequence
        type: u4le

  tx_out:
    seq:
      - id: value
        type: u8le
      - id: script_length
        type: varint
      - id: script
        type: script
        size: script_length

  script:
    seq:
      - id: opcodes
        type: opcode
        repeat: until
        repeat-until: eq(0)

  opcode:
    seq:
      - id: opcode
        type: u1
      - id: data
        type: bytes
        size: varint
        if: opcode > 75 and opcode < 79

  varint:
    seq:
      - id: first_byte
        type: u1
      - id: value_1
        type: u1
        repeat: until
        repeat-until: eq(0)
        if: first_byte > 253
      - id: value_2
        type: u2le
        if: first_byte == 253
      - id: value_3
        type: u4le
        if: first_byte == 254
      - id: value_4
        type: u8le
        if: first_byte == 255