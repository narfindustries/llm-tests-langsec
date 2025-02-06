meta:
  id: bitcoin_transaction
  endian: le
  title: Bitcoin Transaction
  license: MIT

seq:
  - id: version
    type: u4
    doc: Transaction version number

  - id: input_count
    type: vlq_base128_le
    doc: Number of transaction inputs

  - id: inputs
    type: transaction_input
    repeat: expr
    repeat-expr: input_count.value

  - id: output_count
    type: vlq_base128_le
    doc: Number of transaction outputs

  - id: outputs
    type: transaction_output
    repeat: expr
    repeat-expr: output_count.value

  - id: locktime
    type: u4
    doc: Earliest time/block for transaction inclusion

types:
  transaction_input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Hash of previous transaction

      - id: prev_output_index
        type: u4
        doc: Index of previous transaction output

      - id: script_sig_length
        type: vlq_base128_le
        doc: Length of scriptSig

      - id: script_sig
        size: script_sig_length.value
        doc: Cryptographic unlocking script

      - id: sequence_number
        type: u4
        doc: Sequence number for transaction replacement

  transaction_output:
    seq:
      - id: amount
        type: u8
        doc: Amount in satoshis

      - id: script_pubkey_length
        type: vlq_base128_le
        doc: Length of scriptPubKey

      - id: script_pubkey
        size: script_pubkey_length.value
        doc: Locking script defining spending conditions

  vlq_base128_le:
    seq:
      - id: groups
        type: group
        repeat: eos
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
    instances:
      value:
        value: >-
          groups.size > 0 ? (
            groups[0].value +
            (groups.size > 1 ? (groups[1].value << 7) : 0) +
            (groups.size > 2 ? (groups[2].value << 14) : 0) +
            (groups.size > 3 ? (groups[3].value << 21) : 0)
          ) : 0