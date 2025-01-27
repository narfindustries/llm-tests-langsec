meta:
  id: bitcoin_transactions
  title: Bitcoin Transaction Format
  file-extension: dat
  endian: le

seq:
  - id: version
    type: u4
    doc: Transaction version number

  - id: input_count
    type: vlq_base128_be
    doc: Number of transaction inputs

  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count.value
    doc: List of transaction inputs

  - id: output_count
    type: vlq_base128_be
    doc: Number of transaction outputs

  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count.value
    doc: List of transaction outputs

  - id: locktime
    type: u4
    doc: Transaction locktime

types:
  input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Previous transaction hash

      - id: prev_output_index
        type: u4
        doc: Previous output index

      - id: script_length
        type: vlq_base128_be
        doc: Length of scriptSig

      - id: script_sig
        size: script_length.value
        doc: Input script

      - id: sequence
        type: u4
        doc: Sequence number

  output:
    seq:
      - id: value
        type: u8
        doc: Output value in satoshis

      - id: script_length
        type: vlq_base128_be
        doc: Length of scriptPubKey

      - id: script_pub_key
        size: script_length.value
        doc: Output script