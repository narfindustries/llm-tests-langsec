meta:
  id: bitcoin_transaction
  file-extension: bin
  endian: le

seq:
  - id: version
    type: u4
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
  - id: locktime
    type: u4

types:
  varint:
    seq:
      - id: prefix
        type: u1
      - id: value
        type: 
          switch-on: prefix
          cases:
            0xfd: u2
            0xfe: u4
            0xff: u8
    instances:
      value:
        value: |
          prefix < 0xfd ? prefix : 
          prefix == 0xfd ? _root._io.read_u2le :
          prefix == 0xfe ? _root._io.read_u4le :
          _root._io.read_u8le

  transaction_input:
    seq:
      - id: previous_output
        type: outpoint
      - id: script_length
        type: varint
      - id: script_sig
        size: script_length.value
      - id: sequence
        type: u4

  transaction_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: varint
      - id: script_pubkey
        size: script_length.value

  outpoint:
    seq:
      - id: hash
        size: 32
      - id: index
        type: u4