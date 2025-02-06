meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  endian: le
  file-extension: btc

seq:
  - id: version
    type: u4
  - id: in_count
    type: var_int
  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: in_count.value
  - id: out_count
    type: var_int
  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: out_count.value
  - id: lock_time
    type: u4

types:
  tx_input:
    seq:
      - id: prev_tx_hash
        type: u1
        repeat: expr
        repeat-expr: 32  # 256-bit hash
      - id: prev_out_index
        type: u4
      - id: script_len
        type: var_int
      - id: script_sig
        type: u1
        repeat: expr
        repeat-expr: script_len.value
      - id: sequence
        type: u4

  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_len
        type: var_int
      - id: script_pub_key
        type: u1
        repeat: expr
        repeat-expr: script_len.value

  var_int:
    seq:
      - id: len
        type: u1

    instances:
      value:
        value: |
          len < 0xfd ? len :
          len == 0xfd ? _io.read_u2le() :
          len == 0xfe ? _io.read_u4le() :
          _io.read_u8le()
        if: len >= 0xfd