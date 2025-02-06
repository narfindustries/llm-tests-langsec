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
    if: version >= 2
  - id: input_count
    type: var_int
  - id: inputs
    type: tx_in
    repeat: expr
    repeat-expr: input_count.int_value
  - id: output_count
    type: var_int
  - id: outputs
    type: tx_out
    repeat: expr
    repeat-expr: output_count.int_value
  - id: witnesses
    type: witness_data
    repeat: expr
    repeat-expr: input_count.int_value
    if: is_segwit == 0x01
  - id: lock_time
    type: u4

types:
  var_int:
    seq:
      - id: flag
        type: u1
      - id: data
        type:
          switch-on: flag
          cases:
            0xfd: u2
            0xfe: u4
            0xff: u8
        if: flag >= 0xfd
    instances:
      int_value:
        value: 'flag < 0xfd ? flag : data'

  tx_in:
    seq:
      - id: previous_output
        type: outpoint
      - id: script_length
        type: var_int
      - id: script_sig
        size: script_length.int_value
      - id: sequence
        type: u4

  tx_out:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: var_int
      - id: script_pubkey
        size: script_length.int_value

  outpoint:
    seq:
      - id: hash
        size: 32
      - id: index
        type: u4

  witness_data:
    seq:
      - id: witness_count
        type: var_int
      - id: witness_items
        type: witness_item
        repeat: expr
        repeat-expr: witness_count.int_value

  witness_item:
    seq:
      - id: item_length
        type: var_int
      - id: witness_data
        size: item_length.int_value