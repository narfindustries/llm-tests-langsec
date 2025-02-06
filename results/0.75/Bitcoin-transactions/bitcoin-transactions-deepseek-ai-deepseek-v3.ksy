meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: btc
  endian: le
seq:
  - id: version
    type: u4
  - id: marker
    type: u1
    if: _root.version > 1
  - id: flag
    type: u1
    if: _root.version > 1
  - id: num_inputs
    type: vlq_base128_le
  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: num_inputs.value
  - id: num_outputs
    type: vlq_base128_le
  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: num_outputs.value
  - id: witness
    type: witness
    repeat: expr
    repeat-expr: num_inputs.value
    if: _root.marker == 0 and _root.flag == 1
  - id: lock_time
    type: u4
types:
  tx_input:
    seq:
      - id: previous_tx_hash
        type: b32
      - id: previous_tx_output_index
        type: u4
      - id: script_sig_length
        type: vlq_base128_le
      - id: script_sig
        size: script_sig_length.value
        type: str
        encoding: ASCII
      - id: sequence
        type: u4
  tx_output:
    seq:
      - id: value
        type: s8
      - id: script_pub_key_length
        type: vlq_base128_le
      - id: script_pub_key
        size: script_pub_key_length.value
        type: str
        encoding: ASCII
  witness:
    seq:
      - id: num_witness_elements
        type: vlq_base128_le
      - id: witness_elements
        type: witness_element
        repeat: expr
        repeat-expr: num_witness_elements.value
  witness_element:
    seq:
      - id: witness_length
        type: vlq_base128_le
      - id: witness_data
        size: witness_length.value
        type: str
        encoding: ASCII
  b32:
    seq:
      - id: value
        size: 32
        type: str
        encoding: ASCII
  vlq_base128_le:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: _.value & 0x80 == 0