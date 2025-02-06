meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  license: CC0-1.0
  endian: le
seq:
  - id: version
    type: u4
  - id: marker
    type: u1
    if: _root.is_segwit
  - id: flag
    type: u1
    if: _root.is_segwit
  - id: input_count
    type: var_int
  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: input_count.value
  - id: output_count
    type: var_int
  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: output_count.value
  - id: witness_data
    type: witness
    repeat: expr
    repeat-expr: input_count.value
    if: _root.is_segwit
  - id: lock_time
    type: u4
types:
  var_int:
    seq:
      - id: value
        type:
          switch-on: _io.read_u1
          cases:
            0xfd: u2
            0xfe: u4
            0xff: u8
            _: u1
  tx_input:
    seq:
      - id: prev_tx_hash
        size: 32
        type: str
        encoding: HEX
      - id: prev_output_index
        type: u4
      - id: script_length
        type: var_int
      - id: script_sig
        size: script_length.value
        type: str
        encoding: HEX
      - id: sequence
        type: u4
  tx_output:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: var_int
      - id: script_pubkey
        size: script_length.value
        type: str
        encoding: HEX
  witness:
    seq:
      - id: witness_count
        type: var_int
      - id: witness_elements
        type: str
        encoding: HEX
        repeat: expr
        repeat-expr: witness_count.value
instances:
  is_segwit:
    value: marker == 0x00 and flag == 0x01