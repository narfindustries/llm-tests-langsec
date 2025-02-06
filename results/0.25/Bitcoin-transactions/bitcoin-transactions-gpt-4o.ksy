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
    seq:
      - id: first_byte
        type: u1
      - id: value_2
        type: u2le
        if: first_byte == 0xfd
      - id: value_4
        type: u4le
        if: first_byte == 0xfe
      - id: value_8
        type: u8le
        if: first_byte == 0xff

    instances:
      actual_value:
        value: 'first_byte == 0xfd ? value_2 : (first_byte == 0xfe ? value_4 : (first_byte == 0xff ? value_8 : first_byte))'

  input:
    seq:
      - id: prev_tx_hash
        type: b32

      - id: output_index
        type: u4

      - id: script_sig_len
        type: varint

      - id: script_sig
        size: script_sig_len.actual_value

      - id: sequence
        type: u4

  output:
    seq:
      - id: value
        type: u8

      - id: script_pubkey_len
        type: varint

      - id: script_pubkey
        size: script_pubkey_len.actual_value