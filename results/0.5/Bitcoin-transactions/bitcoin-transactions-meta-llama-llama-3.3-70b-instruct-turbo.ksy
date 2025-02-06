meta:
  id: bitcoin_transactions
  endian: le

seq:
  - id: version
    type: u4le
  - id: input_count
    type: varint
  - id: inputs
    type: transaction_input
    repeat: expr
    repeat-expr: input_count.value
  - id: output_count
    type: varint
  - id: outputs
    type: transaction_output
    repeat: expr
    repeat-expr: output_count.value
  - id: lock_time
    type: u4le

types:
  transaction_input:
    seq:
      - id: previous_transaction_hash
        type: bytes
        size: 32
      - id: previous_transaction_index
        type: u4le
      - id: script_length
        type: varint
      - id: script
        type: bytes
        size: expr
        size-expr: script_length.value
      - id: sequence_number
        type: u4le

  transaction_output:
    seq:
      - id: value
        type: u8le
      - id: script_length
        type: varint
      - id: script
        type: bytes
        size: expr
        size-expr: script_length.value

  varint:
    seq:
      - id: first_byte
        type: u1
    if first_byte < 253:
      value: first_byte
    elif first_byte == 253:
      seq:
        - id: value
          type: u2le
    elif first_byte == 254:
      seq:
        - id: value
          type: u4le
    else:
      seq:
        - id: value
          type: u8le

  bytes:
    seq:
      - id: value
        type: byte
        repeat: until
        until: eos