meta:
  id: bitcoin_transaction
  endian: le
seq:
  - id: version
    type: s4
  - id: marker
    type: u1
    if: marker == 0x00
  - id: flag
    type: u1
    if: marker == 0x00
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
  - id: witness
    type: tx_witness
    repeat: expr
    repeat-expr: input_count.value
    if: marker == 0x00
  - id: lock_time
    type: u4
types:
  var_int:
    seq:
      - id: value
        type:
          switch-on: _io.read_u1()
          cases:
            0xFD: u2
            0xFE: u4
            0xFF: u8
            _: u1
  tx_input:
    seq:
      - id: prevout_hash
        size: 32
        type: u1
        repeat: expr
        repeat-expr: 32
      - id: prevout_index
        type: u4
      - id: script_sig_len
        type: var_int
      - id: script_sig
        size: script_sig_len.value
        type: u1
        repeat: expr
        repeat-expr: script_sig_len.value
      - id: sequence
        type: u4
  tx_output:
    seq:
      - id: value
        type: s8
      - id: script_pubkey_len
        type: var_int
      - id: script_pubkey
        size: script_pubkey_len.value
        type: u1
        repeat: expr
        repeat-expr: script_pubkey_len.value
  tx_witness:
    seq:
      - id: witness_count
        type: var_int
      - id: witness_elements
        type: witness_element
        repeat: expr
        repeat-expr: witness_count.value
  witness_element:
    seq:
      - id: length
        type: var_int
      - id: data
        size: length.value
        type: u1
        repeat: expr
        repeat-expr: length.value