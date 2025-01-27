meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: btc
  endian: le
  license: CC0-1.0
  ks-version: 0.9

doc: |
  A Bitcoin transaction tells the network that the owner of some bitcoin
  value has authorized the transfer of that value to another owner.
  The new owner can now spend the bitcoins by creating another transaction
  that authorizes transfer to another owner, and so on, in a chain of ownership.

seq:
  - id: version
    type: u4
    doc: Transaction version number.

  - id: tx_in_count
    type: vlq_base128_le
    doc: Number of Transaction inputs.

  - id: tx_in
    type: tx_in
    repeat: expr
    repeat-expr: tx_in_count.value
    doc: List of transaction inputs.

  - id: tx_out_count
    type: vlq_base128_le
    doc: Number of Transaction outputs.

  - id: tx_out
    type: tx_out
    repeat: expr
    repeat-expr: tx_out_count.value
    doc: List of transaction outputs.

  - id: lock_time
    type: u4
    doc: A time or block number which defines the earliest time that this transaction can be added to the block chain.

types:
  tx_in:
    seq:
      - id: previous_output
        type: out_point
        doc: The previous output transaction reference.

      - id: script_length
        type: vlq_base128_le
        doc: Length of the script.

      - id: script_sig
        type: b
        size: script_length.value
        doc: Computational Script for confirming transaction authorization.

      - id: sequence
        type: u4
        doc: Originally intended for "high-frequency trades", with every increase in the sequence number signaling a newer version of the transaction.

  out_point:
    seq:
      - id: hash
        type: b
        size: 32
        doc: The hash of the referenced transaction.

      - id: index
        type: u4
        doc: The index of the specific output in the transaction. The first output is 0, etc.

  tx_out:
    seq:
      - id: value
        type: u8
        doc: Transaction value in satoshis.

      - id: pk_script_length
        type: vlq_base128_le
        doc: Length of the public key script.

      - id: pk_script
        type: b
        size: pk_script_length.value
        doc: Usually contains the public key as a Bitcoin script setting up conditions to claim this output.