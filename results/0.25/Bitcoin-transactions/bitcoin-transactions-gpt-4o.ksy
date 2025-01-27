meta:
  id: bitcoin_transactions
  title: Bitcoin Transactions
  file-extension: bin
  endian: little

seq:
  - id: version
    type: u4

  - id: input_count
    type: vlq_base128_le
    doc: Number of transaction inputs

  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count

  - id: output_count
    type: vlq_base128_le
    doc: Number of transaction outputs

  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count

  - id: lock_time
    type: u4

types:
  input:
    seq:
      - id: previous_output
        type: outpoint

      - id: script_length
        type: vlq_base128_le

      - id: script_sig
        size: script_length

      - id: sequence
        type: u4

  output:
    seq:
      - id: value
        type: u8

      - id: pk_script_length
        type: vlq_base128_le

      - id: pk_script
        size: pk_script_length

  outpoint:
    seq:
      - id: txid
        type: bytes
        size: 32
        doc: Transaction ID of the previous output

      - id: index
        type: u4
        doc: Output index in the previous transaction