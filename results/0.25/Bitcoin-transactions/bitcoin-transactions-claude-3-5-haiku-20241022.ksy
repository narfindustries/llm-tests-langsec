meta:
  id: bitcoin_transaction
  endian: le
  title: Bitcoin Transaction Structure
  license: MIT

seq:
  - id: version
    type: u4
    doc: Transaction version number

  - id: tx_in_count
    type: vlq_base128_le
    doc: Number of transaction inputs

  - id: inputs
    type: transaction_input
    repeat: expr
    repeat-expr: tx_in_count.value
    doc: List of transaction inputs

  - id: tx_out_count
    type: vlq_base128_le
    doc: Number of transaction outputs

  - id: outputs
    type: transaction_output
    repeat: expr
    repeat-expr: tx_out_count.value
    doc: List of transaction outputs

  - id: locktime
    type: u4
    doc: Transaction locktime (block height or timestamp)

types:
  transaction_input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Hash of previous transaction

      - id: prev_output_index
        type: u4
        doc: Index of output in previous transaction

      - id: script_sig_length
        type: vlq_base128_le
        doc: Length of scriptSig

      - id: script_sig
        size: script_sig_length.value
        doc: Unlocking script

      - id: sequence_number
        type: u4
        doc: Sequence number for input

  transaction_output:
    seq:
      - id: amount
        type: s8
        doc: Amount in satoshis

      - id: script_pubkey_length
        type: vlq_base128_le
        doc: Length of scriptPubKey

      - id: script_pubkey
        size: script_pubkey_length.value
        doc: Locking script

  vlq_base128_le:
    seq:
      - id: groups
        type: group
        repeat: until
        repeat-until: not _.has_next
    types:
      group:
        seq:
          - id: byte
            type: u1
        instances:
          has_next:
            value: (byte & 0x80) != 0
          value:
            value: byte & 0x7f
            if: not has_next

    instances:
      value:
        value: >-
          groups.size > 0 ? 
          (groups[0].value & 0x7f) +
          (groups.size > 1 ? (groups[1].value & 0x7f) << 7 : 0) +
          (groups.size > 2 ? (groups[2].value & 0x7f) << 14 : 0) +
          (groups.size > 3 ? (groups[3].value & 0x7f) << 21 : 0) :
          0