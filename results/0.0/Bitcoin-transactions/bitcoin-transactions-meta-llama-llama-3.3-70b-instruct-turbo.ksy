meta:
  id: bitcoin-transactions
  file-extension: dat
  endian: le
seq:
  - id: transaction_count
    type: u32
  - id: transactions
    type: transaction
    repeat: expr
    repeat-expr: transaction_count
types:
  transaction:
    seq:
      - id: version
        type: u32
      - id: tx_in_count
        type: u32
      - id: tx_ins
        type: tx_in
        repeat: expr
        repeat-expr: tx_in_count
      - id: tx_out_count
        type: u32
      - id: tx_outs
        type: tx_out
        repeat: expr
        repeat-expr: tx_out_count
      - id: lock_time
        type: u32
    types:
      tx_in:
        seq:
          - id: prev_tx_hash
            type: bytes
            size: 32
          - id: prev_tx_out_index
            type: u32
          - id: script_length
            type: u32
          - id: script
            type: bytes
            size: script_length
          - id: sequence
            type: u32
        padding: 4
      tx_out:
        seq:
          - id: value
            type: u64
          - id: script_length
            type: u32
          - id: script
            type: bytes
            size: script_length
        padding: 4