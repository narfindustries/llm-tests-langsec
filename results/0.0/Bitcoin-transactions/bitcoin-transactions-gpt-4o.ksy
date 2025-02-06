meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: bin
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
      - id: value
        type: u1
      - id: value_16
        type: u2
        if: value == 0xfd
      - id: value_32
        type: u4
        if: value == 0xfe
      - id: value_64
        type: u8
        if: value == 0xff

    instances:
      actual_value:
        value: 'value == 0xfd ? value_16 : (value == 0xfe ? value_32 : (value == 0xff ? value_64 : value))'

  input:
    seq:
      - id: prev_tx_hash
        type: str
        size: 32
        encoding: ascii

      - id: output_index
        type: u4

      - id: script_sig_len
        type: varint

      - id: script_sig
        type: str
        size: script_sig_len.actual_value
        encoding: ascii

      - id: sequence
        type: u4

  output:
    seq:
      - id: value
        type: u8

      - id: script_pubkey_len
        type: varint

      - id: script_pubkey
        type: str
        size: script_pubkey_len.actual_value
        encoding: ascii