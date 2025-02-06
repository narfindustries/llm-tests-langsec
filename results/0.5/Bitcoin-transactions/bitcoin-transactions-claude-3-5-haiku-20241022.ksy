meta:
  id: bitcoin_transaction
  endian: le
  encoding: utf-8

seq:
  - id: version
    type: u4
    doc: Transaction version number

  - id: marker
    type: u1
    if: tx_in_count.is_non_zero
    doc: SegWit marker byte

  - id: flag
    type: u1
    if: marker == 0
    doc: SegWit flag byte

  - id: tx_in_count
    type: vlq_base128_be
    doc: Number of transaction inputs

  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: tx_in_count.value
    doc: List of transaction inputs

  - id: tx_out_count
    type: vlq_base128_be
    doc: Number of transaction outputs

  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: tx_out_count.value
    doc: List of transaction outputs

  - id: witness_data
    type: witness_stack
    if: marker == 0
    doc: Optional witness data for SegWit transactions

  - id: locktime
    type: u4
    doc: Transaction locktime (block height or timestamp)

types:
  vlq_base128_be:
    seq:
      - id: groups
        type: vlq_group
        repeat: until
        repeat-until: not _.has_next
    types:
      vlq_group:
        seq:
          - id: has_next
            type: b1
          - id: value
            type: b7
    instances:
      value:
        value: '_calc_value'
      is_non_zero:
        value: 'value > 0'
      _calc_value:
        value: >-
          groups.fold(0, lambda acc, g: (acc << 7) | g.value)

  tx_input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Hash of previous transaction

      - id: prev_output_index
        type: u4
        doc: Index of output in previous transaction

      - id: script_sig_length
        type: vlq_base128_be
        doc: Length of scriptSig

      - id: script_sig
        size: script_sig_length.value
        doc: Unlocking script

      - id: sequence_number
        type: u4
        doc: Sequence number for input

  tx_output:
    seq:
      - id: amount
        type: s8
        doc: Amount in satoshis

      - id: script_pubkey_length
        type: vlq_base128_be
        doc: Length of scriptPubKey

      - id: script_pubkey
        size: script_pubkey_length.value
        doc: Locking script

  witness_stack:
    seq:
      - id: witness_count
        type: vlq_base128_be
        doc: Number of witness items

      - id: witness_items
        type: witness_item
        repeat: expr
        repeat-expr: witness_count.value
        doc: Witness data for each input

  witness_item:
    seq:
      - id: witness_length
        type: vlq_base128_be
        doc: Length of witness data

      - id: witness_data
        size: witness_length.value
        doc: Witness stack data

instances:
  is_segwit:
    value: 'marker == 0'
    doc: Check if transaction is SegWit