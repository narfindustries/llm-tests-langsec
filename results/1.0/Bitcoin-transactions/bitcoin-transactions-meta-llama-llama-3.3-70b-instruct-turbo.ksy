meta:
  id: bitcoin_transactions
  title: Bitcoin transactions
  license: mit
  encoding: UTF-8

seq:
  - id: transaction
    type: transaction
    repeat: eos

types:
  transaction:
    seq:
      - id: header
        type: transaction_header
      - id: inputs
        type: tx_in
        repeat: varint
      - id: outputs
        type: tx_out
        repeat: varint

  transaction_header:
    seq:
      - id: version
        type: u4
      - id: num_inputs
        type: varint
      - id: num_outputs
        type: varint
      - id: locktime
        type: u4

  tx_in:
    seq:
      - id: prev_out
        type: outpoint
      - id: script_length
        type: varint
      - id: script_sig
        type: bytes
        size: script_length
      - id: sequence
        type: u4

  tx_out:
    seq:
      - id: value
        type: u8
      - id: pk_script_length
        type: varint
      - id: pk_script
        type: bytes
        size: pk_script_length

  outpoint:
    seq:
      - id: hash
        type: bytes
        size: 32
      - id: index
        type: u4