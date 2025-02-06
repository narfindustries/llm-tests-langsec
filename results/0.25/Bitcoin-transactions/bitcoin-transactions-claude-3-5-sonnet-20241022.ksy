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
  - id: flag
    type: u1
    if: is_segwit == 1
  - id: input_count
    type: varint
  - id: inputs
    type: tx_in
    repeat: expr
    repeat-expr: input_count.final_value
  - id: output_count
    type: varint
  - id: outputs
    type: tx_out
    repeat: expr
    repeat-expr: output_count.final_value
  - id: witness_data
    type: witness
    repeat: expr
    repeat-expr: input_count.final_value
    if: is_segwit == 1
  - id: lock_time
    type: u4

types:
  varint:
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

  tx_in:
    seq:
      - id: previous_tx_hash
        size: 32
      - id: previous_tx_index
        type: u4
      - id: script_sig_length
        type: varint
      - id: script_sig
        size: script_sig_length.final_value
      - id: sequence
        type: u4

  tx_out:
    seq:
      - id: value
        type: u8
      - id: script_pubkey_length
        type: varint
      - id: script_pubkey
        size: script_pubkey_length.final_value

  witness:
    seq:
      - id: witness_count
        type: varint
      - id: items
        type: witness_item
        repeat: expr
        repeat-expr: witness_count.final_value

  witness_item:
    seq:
      - id: size
        type: varint
      - id: data
        size: size.final_value

enums:
  sighash_type:
    1: all
    2: none
    3: single
    0x80: anyone_can_pay