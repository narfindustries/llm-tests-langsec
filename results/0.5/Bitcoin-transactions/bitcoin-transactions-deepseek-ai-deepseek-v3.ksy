meta:
  id: bitcoin_transaction
  file-extension: tx
  endian: le
seq:
  - id: version
    type: u4
  - id: num_inputs
    type: vlq_base128_le
  - id: inputs
    type: input
    repeat: expr
    repeat-expr: num_inputs.result
  - id: num_outputs
    type: vlq_base128_le
  - id: outputs
    type: output
    repeat: expr
    repeat-expr: num_outputs.result
  - id: locktime
    type: u4
  - id: witness
    type: witness
    if: _root.version == 2
types:
  input:
    seq:
      - id: previous_tx_hash
        size: 32
      - id: previous_output_index
        type: u4
      - id: len_script_sig
        type: vlq_base128_le
      - id: script_sig
        size: len_script_sig.result
      - id: sequence
        type: u4
  output:
    seq:
      - id: value
        type: u8
      - id: len_script_pubkey
        type: vlq_base128_le
      - id: script_pubkey
        size: len_script_pubkey.result
  witness:
    seq:
      - id: num_witness_fields
        type: vlq_base128_le
      - id: witness_fields
        type: witness_field
        repeat: expr
        repeat-expr: num_witness_fields.result
  witness_field:
    seq:
      - id: len_witness_field_data
        type: vlq_base128_le
      - id: witness_field_data
        size: len_witness_field_data.result
  vlq_base128_le:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: _.value & 0x80 == 0
    instances:
      result:
        value: >
          (value[0] & 0x7F) |
          ((value[1] & 0x7F) << 7) |
          ((value[2] & 0x7F) << 14) |
          ((value[3] & 0x7F) << 21) |
          ((value[4] & 0x7F) << 28) |
          ((value[5] & 0x7F) << 35) |
          ((value[6] & 0x7F) << 42) |
          ((value[7] & 0x7F) << 49) |
          ((value[8] & 0x7F) << 56) |
          ((value[9] & 0x7F) << 63)