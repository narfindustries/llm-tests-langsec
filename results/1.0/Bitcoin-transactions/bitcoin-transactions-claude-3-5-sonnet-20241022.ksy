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
    type: tx_input
    repeat: expr
    repeat-expr: input_count.value
  - id: output_count 
    type: varint
  - id: outputs
    type: tx_output  
    repeat: expr
    repeat-expr: output_count.value
  - id: locktime
    type: u4

types:
  varint:
    seq:
      - id: byte1
        type: u1
      - id: byte2_8
        type: u8
        if: byte1 == 0xff
      - id: byte2_4 
        type: u4
        if: byte1 == 0xfe
      - id: byte2_2
        type: u2
        if: byte1 == 0xfd
    instances:
      value:
        value: >-
          byte1 == 0xff ? byte2_8 : 
          byte1 == 0xfe ? byte2_4 :
          byte1 == 0xfd ? byte2_2 : 
          byte1

  tx_input:
    seq:
      - id: previous_output
        type: outpoint
      - id: script_length
        type: varint
      - id: signature_script
        size: script_length.value
      - id: sequence
        type: u4

  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: varint
      - id: pubkey_script
        size: script_length.value

  outpoint:
    seq:
      - id: hash
        size: 32
      - id: index
        type: u4