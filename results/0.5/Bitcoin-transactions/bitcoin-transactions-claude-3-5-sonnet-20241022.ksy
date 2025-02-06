meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: btc
  endian: le

seq:
  - id: version
    type: u4
  - id: is_segwit
    type: segwit_marker
    if: version >= 2
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
  - id: witnesses
    type: witness_data
    repeat: expr
    repeat-expr: input_count.final_value
    if: is_segwit.is_segwit
  - id: locktime
    type: u4

types:
  segwit_marker:
    seq:
      - id: marker
        type: u1
      - id: flag
        type: u1
    instances:
      is_segwit:
        value: marker == 0 and flag == 1

  var_int:
    seq:
      - id: prefix
        type: u1
      - id: value_u2
        type: u2
        if: prefix == 0xfd
      - id: value_u4
        type: u4
        if: prefix == 0xfe
      - id: value_u8
        type: u8
        if: prefix == 0xff
    instances:
      final_value:
        value: >-
          prefix < 0xfd ? prefix :
          prefix == 0xfd ? value_u2 :
          prefix == 0xfe ? value_u4 :
          value_u8

  tx_input:
    seq:
      - id: previous_output_hash
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

  witness_data:
    seq:
      - id: witness_count
        type: var_int
      - id: witness_items
        type: witness_item
        repeat: expr
        repeat-expr: witness_count.final_value

  witness_item:
    seq:
      - id: item_length
        type: var_int
      - id: witness_data
        size: item_length.final_value