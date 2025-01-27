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
        type: uvarint
      - id: tx_ins
        type: tx_in
        repeat: expr
        repeat-expr: tx_in_count
      - id: tx_out_count
        type: uvarint
      - id: tx_outs
        type: tx_out
        repeat: expr
        repeat-expr: tx_out_count
      - id: lock_time
        type: u32
    types:
      tx_in:
        seq:
          - id: prev_out
            type: prev_out
          - id: script_length
            type: uvarint
          - id: script_sig
            type: str
            size: script_length
          - id: sequence
            type: u32
        types:
          prev_out:
            seq:
              - id: tx_id
                type: bytes
                size: 32
              - id: out_idx
                type: u32
      tx_out:
        seq:
          - id: value
            type: u64
          - id: pk_script_length
            type: uvarint
          - id: pk_script
            type: str
            size: pk_script_length