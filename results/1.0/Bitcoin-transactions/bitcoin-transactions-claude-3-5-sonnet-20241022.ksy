meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction Format
  file-extension: btc
  endian: le

seq:
  - id: version
    type: u4
  - id: is_segwit
    type: b1
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
    if: is_segwit == true
  - id: lock_time
    type: u4

types:
  var_int:
    seq:
      - id: prefix
        type: u1
      - id: raw_value
        type:
          switch-on: prefix
          cases:
            0xfd: u2
            0xfe: u4
            0xff: u8
    instances:
      final_value:
        value: 'prefix <= 0xfc ? prefix : raw_value'

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
      - id: script_pubkey
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

enums:
  tx_version:
    1: legacy
    2: segwit