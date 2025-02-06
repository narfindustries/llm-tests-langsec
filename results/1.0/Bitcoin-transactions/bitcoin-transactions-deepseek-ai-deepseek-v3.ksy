meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction Specification
  license: CC0-1.0
  ks-version: 0.9
seq:
  - id: version
    type: u4le
    doc: "Transaction version number (e.g., 1 or 2 for SegWit)"
  - id: marker
    type: u1
    if: _root.version == 2
    doc: "Marker byte (0x00) for SegWit transactions"
  - id: flag
    type: u1
    if: _root.version == 2
    doc: "Flag byte (0x01) for SegWit transactions"
  - id: tx_in_count
    type: varint
    doc: "Number of transaction inputs"
  - id: inputs
    type: input
    repeat: expr
    repeat-expr: tx_in_count
    doc: "List of transaction inputs"
  - id: tx_out_count
    type: varint
    doc: "Number of transaction outputs"
  - id: outputs
    type: output
    repeat: expr
    repeat-expr: tx_out_count
    doc: "List of transaction outputs"
  - id: witness
    type: witness_data
    if: _root.version == 2
    repeat: expr
    repeat-expr: tx_in_count
    doc: "Witness data for SegWit transactions"
  - id: lock_time
    type: u4le
    doc: "Lock time for the transaction"
types:
  varint:
    seq:
      - id: value
        type:
          switch-on: io.read_u1()
          cases:
            0xFD: u2le
            0xFE: u4le
            0xFF: u8le
        doc: "Variable-length integer encoding"
  input:
    seq:
      - id: previous_txid
        size: 32
        type: u1
        repeat: expr
        repeat-expr: 32
        doc: "Hash of the previous transaction output"
      - id: previous_index
        type: u4le
        doc: "Index of the previous transaction output"
      - id: script_length
        type: varint
        doc: "Length of the script signature"
      - id: script_sig
        size: script_length.value
        type: u1
        repeat: expr
        repeat-expr: script_length.value
        doc: "Script signature"
      - id: sequence
        type: u4le
        doc: "Sequence number"
  output:
    seq:
      - id: value
        type: u8le
        doc: "Amount of Bitcoin in Satoshis"
      - id: script_length
        type: varint
        doc: "Length of the script public key"
      - id: script_pubkey
        size: script_length.value
        type: u1
        repeat: expr
        repeat-expr: script_length.value
        doc: "Script public key"
  witness_data:
    seq:
      - id: witness_count
        type: varint
        doc: "Number of witness elements"
      - id: witness_data
        size: witness_count.value
        type: u1
        repeat: expr
        repeat-expr: witness_count.value
        doc: "Witness data (e.g., signatures)"