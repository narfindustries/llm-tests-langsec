meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  endian: le
seq:
  - id: version
    type: u4
  - id: input_count
    type: varint
  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count.actual_value
  - id: output_count
    type: varint
  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count.actual_value
  - id: locktime
    type: u4

types:
  varint:
    doc: "Variable-length integer as used in Bitcoin protocol"
    seq:
      - id: first_byte
        type: u1
      - id: value
        type:
          switch-on: first_byte
          cases:
            0xfd: u2
            0xfe: u4
            0xff: u8
        if: first_byte >= 0xfd
    instances:
      actual_value:
        value: 'first_byte < 0xfd ? first_byte : value'

  input:
    seq:
      - id: prev_tx_hash
        type: b32
      - id: output_index
        type: u4
      - id: scriptsig_len
        type: varint
      - id: scriptsig
        size: scriptsig_len.actual_value
      - id: sequence
        type: u4

  output:
    seq:
      - id: value
        type: u8
      - id: scriptpubkey_len
        type: varint
      - id: scriptpubkey
        size: scriptpubkey_len.actual_value