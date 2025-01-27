meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: tx
  endian: le

seq:
  - id: version
    type: u4
    doc: Transaction version number

  - id: tx_in_count
    type: varint
    doc: Number of transaction inputs

  - id: inputs
    type: transaction_input
    repeat: expr
    repeat-expr: tx_in_count.value
    doc: List of transaction inputs

  - id: tx_out_count
    type: varint
    doc: Number of transaction outputs

  - id: outputs
    type: transaction_output
    repeat: expr
    repeat-expr: tx_out_count.value
    doc: List of transaction outputs

  - id: locktime
    type: u4
    doc: Transaction locktime

types:
  transaction_input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Hash of the previous transaction

      - id: prev_tx_index
        type: u4
        doc: Index of the output in the previous transaction

      - id: script_sig_length
        type: varint
        doc: Length of the scriptSig

      - id: script_sig
        size: script_sig_length.value
        doc: Signature script

      - id: sequence
        type: u4
        doc: Sequence number

  transaction_output:
    seq:
      - id: value
        type: u8
        doc: Amount of satoshis to be spent

      - id: script_pub_key_length
        type: varint
        doc: Length of the scriptPubKey

      - id: script_pub_key
        size: script_pub_key_length.value
        doc: Public key script

  varint:
    seq:
      - id: single_byte
        type: u1
        if: value < 0xfd

      - id: two_bytes
        type: u2
        if: value == 0xfd

      - id: four_bytes
        type: u4
        if: value == 0xfe

      - id: eight_bytes
        type: u8
        if: value == 0xff
    instances:
      value:
        value: >-
          single_byte != 0 ? single_byte :
          two_bytes != 0 ? two_bytes :
          four_bytes != 0 ? four_bytes :
          eight_bytes