meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  endian: le
doc: |
  Structure of a Bitcoin transaction

seq:
  - id: version
    type: u4
  - id: input_count
    type: vlq_base128_le
  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count.value
  - id: output_count
    type: vlq_base128_le
  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count.value
  - id: locktime
    type: u4

types:
  input:
    seq:
      - id: prev_tx_hash
        type: u8
        repeat: expr
        repeat-expr: 4
      - id: output_index
        type: u4
      - id: script_length
        type: vlq_base128_le
      - id: script_sig
        size: script_length.value
      - id: sequence
        type: u4
  output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: vlq_base128_le
      - id: script_pub_key
        size: script_length.value

  vlq_base128_le:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: (_ & 0x80) == 0
    instances:
      value:
        value: |
          (groups.size > 0 ? ((groups[0] & 0x7F)) : 0) +
          (groups.size > 1 ? ((groups[1] & 0x7F) << 7) : 0) +
          (groups.size > 2 ? ((groups[2] & 0x7F) << 14) : 0) +
          (groups.size > 3 ? ((groups[3] & 0x7F) << 21) : 0) +
          (groups.size > 4 ? ((groups[4] & 0x7F) << 28) : 0)
    doc: |
      Variable-length quantity (VLQ) in little-endian form, used to encode integer values for better compression. Commonly used in Bitcoin's protocol.