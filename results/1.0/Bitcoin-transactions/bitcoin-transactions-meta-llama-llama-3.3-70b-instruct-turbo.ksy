meta:
  id: bitcoin_transactions
  endian: le

seq:
  - id: version
    type: u4
  - id: tx_in_count
    type: var_int
  - id: tx_ins
    type: tx_in
    repeat: expr
      tx_in_count
  - id: tx_out_count
    type: var_int
  - id: tx_outs
    type: tx_out
    repeat: expr
      tx_out_count
  - id: lock_time
    type: u4

types:
  tx_in:
    seq:
      - id: previous_output
        type: prev_out
      - id: script_length
        type: var_int
      - id: script
        type: bytes
        size: script_length
      - id: sequence
        type: u4
  tx_out:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: var_int
      - id: script
        type: bytes
        size: script_length
  prev_out:
    seq:
      - id: txid
        type: bytes
        size: 32
      - id: vout
        type: u4
  var_int:
    seq:
      - id: fd
        type: u1
      - id: value
        type:
          switch_on: fd
          cases:
            - 0xfd: u2
            - 0xfe: u4
            - 0xff: u8