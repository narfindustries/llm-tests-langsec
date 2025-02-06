meta:
  id: bitcoin_transactions
  endian: le

seq:
  - id: version
    type: u4
  - id: tx_in_count
    type: varint
  - id: tx_ins
    type: tx_in
    repeat: expr
    repeat-expr: tx_in_count
  - id: tx_out_count
    type: varint
  - id: tx_outs
    type: tx_out
    repeat: expr
    repeat-expr: tx_out_count
  - id: lock_time
    type: u4

types:
  tx_in:
    seq:
      - id: previous_output_hash
        type: bytes
        size: 32
      - id: previous_output_index
        type: u4
      - id: script_length
        type: varint
      - id: script_sig
        type: bytes
        size: script_length
      - id: sequence_number
        type: u4
      - id: witness
        type: witness
        if: script_length == 0

  tx_out:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: varint
      - id: script_pub_key
        type: bytes
        size: script_length

  witness:
    seq:
      - id: witness_item_count
        type: varint
      - id: witness_items
        type: bytes
        repeat: expr
        repeat-expr: witness_item_count

  varint:
    seq:
      - id: first_byte
        type: u1
      - id: value
        type:
          switch-on: first_byte
          cases:
            0xfd:
              type: u2
            0xfe:
              type: u4
            0xff:
              type: u8
            default:
              type: u1