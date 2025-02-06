meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  endian: le

seq:
  - id: version
    type: u4

  - id: tx_in_count
    type: var_int

  - id: inputs
    type: tx_in
    repeat: expr
    repeat-expr: tx_in_count.value

  - id: tx_out_count
    type: var_int

  - id: outputs
    type: tx_out
    repeat: expr
    repeat-expr: tx_out_count.value

  - id: lock_time
    type: u4

types:
  var_int:
    seq:
      - id: prefix
        type: u1
      - id: value
        type: u1
        if: prefix == 0xfd
      - id: value_2
        type: u2
        if: prefix == 0xfd
      - id: value_4
        type: u4
        if: prefix == 0xfe
      - id: value_8
        type: u8
        if: prefix == 0xff
    instances:
      value:
        value: |
          switch (prefix) {
            0xfd: value_2;
            0xfe: value_4;
            0xff: value_8;
            else: prefix;
          }

  tx_in:
    seq:
      - id: previous_transaction_hash
        type: bytes
        size: 32

      - id: previous_tx_out_index
        type: u4

      - id: script_sig_len
        type: var_int

      - id: script_sig
        type: bytes
        size: script_sig_len.value

      - id: sequence
        type: u4

  tx_out:
    seq:
      - id: value
        type: u8

      - id: script_pubkey_len
        type: var_int

      - id: script_pubkey
        type: bytes
        size: script_pubkey_len.value