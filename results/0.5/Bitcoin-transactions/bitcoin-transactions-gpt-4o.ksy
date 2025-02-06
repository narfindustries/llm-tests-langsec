meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
seq:
  - id: version
    type: u4le
  - id: input_count
    type: varint
  - id: inputs
    type: input
    repeat: expr
    repeat-expr: input_count.value_decoded
  - id: output_count
    type: varint
  - id: outputs
    type: output
    repeat: expr
    repeat-expr: output_count.value_decoded
  - id: lock_time
    type: u4le

types:
  varint:
    seq:
      - id: value
        type: u1
    instances:
      value_decoded:
        value: |
          value == 0xfd ? _io.read_u2le() :
          value == 0xfe ? _io.read_u4le() :
          value == 0xff ? _io.read_u8le() :
          value

  input:
    seq:
      - id: prev_tx_hash
        type: bytes
        size: 32
      - id: output_index
        type: u4le
      - id: script_sig_len
        type: varint
      - id: script_sig
        size: script_sig_len.value_decoded
      - id: sequence
        type: u4le

  output:
    seq:
      - id: value
        type: u8le
      - id: script_pubkey_len
        type: varint
      - id: script_pubkey
        size: script_pubkey_len.value_decoded