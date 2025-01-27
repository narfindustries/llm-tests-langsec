meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  license: MIT
  ks-version: 0.9
  endian: le
seq:
  - id: version
    type: u4
  - id: input_count
    type: u1
  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: input_count
  - id: output_count
    type: u1
  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: output_count
  - id: lock_time
    type: u4
types:
  tx_input:
    seq:
      - id: previous_output_hash
        size: 32
      - id: previous_output_index
        type: u4
      - id: script_length
        type: u1
      - id: signature_script
        size: script_length
      - id: sequence
        type: u4
  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: u1
      - id: pk_script
        size: script_length