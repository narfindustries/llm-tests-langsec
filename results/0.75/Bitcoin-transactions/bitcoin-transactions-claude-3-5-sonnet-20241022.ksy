meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: btc
  endian: le

seq:
  - id: version
    type: u4
  - id: is_segwit
    type: u1
    if: version == 1
  - id: input_count
    type: var_int
  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: input_count.final_value
  - id: output_count
    type: var_int
  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: output_count.final_value
  - id: witness_data
    type: witness
    if: is_segwit == 0x00
  - id: locktime
    type: u4

types:
  var_int:
    seq:
      - id: flag
        type: u1
      - id: value_u2
        type: u2
        if: flag == 0xfd
      - id: value_u4
        type: u4
        if: flag == 0xfe
      - id: value_u8
        type: u8
        if: flag == 0xff
    instances:
      final_value:
        value: >
          flag < 0xfd ? flag :
          flag == 0xfd ? value_u2 :
          flag == 0xfe ? value_u4 :
          value_u8

  tx_input:
    seq:
      - id: previous_tx_hash
        size: 32
      - id: previous_output_index
        type: u4
      - id: script_length
        type: var_int
      - id: script_sig
        size: script_length.final_value
      - id: sequence
        type: u4

  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: var_int
      - id: script_pub_key
        size: script_length.final_value

  witness:
    seq:
      - id: count
        type: var_int
      - id: items
        type: witness_item
        repeat: expr
        repeat-expr: count.final_value

  witness_item:
    seq:
      - id: length
        type: var_int
      - id: data
        size: length.final_value