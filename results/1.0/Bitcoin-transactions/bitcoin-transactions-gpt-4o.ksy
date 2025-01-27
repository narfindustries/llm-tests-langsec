meta:
  id: bitcoin_transactions
  title: Bitcoin Transactions
  endian: little
  file-extension: btc
  encoding: utf-8

seq:
  - id: version
    type: u4
  - id: inputs
    type: varint
  - repeat: expr
    repeat-expr: inputs
    type: tx_in
  - id: outputs
    type: varint
  - repeat: expr
    repeat-expr: outputs
    type: tx_out
  - id: lock_time
    type: u4

types:
  varint:
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
        if: first_byte != 0xfd and first_byte != 0xfe and first_byte != 0xff
      - id: value
        type: u1
        if: first_byte == 0xfd or first_byte == 0xfe or first_byte == 0xff

  tx_in:
    seq:
      - id: prev_output
        type: outpoint
      - id: script_len
        type: varint
      - id: script_sig
        size: script_len.value
      - id: sequence
        type: u4

  outpoint:
    seq:
      - id: hash
        type: bytes
        size: 32
      - id: index
        type: u4

  tx_out:
    seq:
      - id: value
        type: u8
      - id: pk_script_len
        type: varint
      - id: pk_script
        size: pk_script_len.value