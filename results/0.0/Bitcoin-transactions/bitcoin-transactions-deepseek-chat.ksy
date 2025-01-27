meta:
  id: bitcoin_transaction
  file-extension: bitcoin-tx
  endian: le
  imports:
    - bitcoin_script

seq:
  - id: version
    type: u4
  - id: in_count
    type: vlq_base128_le
  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: in_count
  - id: out_count
    type: vlq_base128_le
  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: out_count
  - id: lock_time
    type: u4

types:
  tx_input:
    seq:
      - id: previous_output_hash
        type: u8
      - id: previous_output_index
        type: u4
      - id: script_length
        type: vlq_base128_le
      - id: signature_script
        type: bitcoin_script
        size: script_length
      - id: sequence
        type: u4

  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: vlq_base128_le
      - id: pk_script
        type: bitcoin_script
        size: script_length

  bitcoin_script:
    seq:
      - id: opcode
        type: u1
      - id: data
        type: bytes
        size-eos: true