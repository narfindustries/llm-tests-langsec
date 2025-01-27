meta:
  id: bitcoin-transactions
  file-extension: dat
  title: Bitcoin Transactions
  license: MIT
  author: Meta Llama 3
  doc: https://en.bitcoin.it/wiki/Protocol_documentation
seq:
  - id: transaction_count
    type: uvarint
  - id: transactions
    type: transaction
    repeat: expr
    repeat-expr: transaction_count
types:
  transaction:
    seq:
      - id: version
        type: u4
      - id: marker
        type: u1
        enum: [0x00]
      - id: flag
        type: u1
        enum: [0x01]
      - id: inputs
        type: tx_in
        repeat: expr
        repeat-expr: varint
      - id: outputs
        type: tx_out
        repeat: expr
        repeat-expr: varint
      - id: witness
        type: witness
        repeat: expr
        repeat-expr: inputs.length
      - id: lock_time
        type: u4
  tx_in:
    seq:
      - id: prev_out
        type: outpoint
      - id: script_length
        type: varint
      - id: script_sig
        type: script
      - id: sequence
        type: u4
  tx_out:
    seq:
      - id: value
        type: u8
      - id: pk_script_length
        type: varint
      - id: pk_script
        type: script
  outpoint:
    seq:
      - id: tx_hash
        type: bytes
        size: 32
      - id: index
        type: u4
  script:
    seq:
      - id: data
        type: byte[]
        size: script_length
  witness:
    seq:
      - id: items
        type: byte[]
        repeat: expr
        repeat-expr: varint