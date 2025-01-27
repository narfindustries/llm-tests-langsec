meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: btc
  endian: le
  license: CC0-1.0
  ks-version: 0.9

doc: |
  A Bitcoin transaction tells the network that the owner of some bitcoin value has
  authorized the transfer of that value to another owner. The new owner can now spend
  the bitcoin by creating another transaction that authorizes transfer to another owner,
  and so on, in a chain of ownership.

seq:
  - id: version
    type: u4
    doc: Transaction version number.

  - id: tx_in_count
    type: vlq_base128_le
    doc: Number of input transactions.

  - id: tx_in
    type: tx_in
    repeat: expr
    repeat-expr: tx_in_count.value

  - id: tx_out_count
    type: vlq_base128_le
    doc: Number of output transactions.

  - id: tx_out
    type: tx_out
    repeat: expr
    repeat-expr: tx_out_count.value

  - id: lock_time
    type: u4
    doc: A time or block number which determines when this transaction is locked.

types:
  tx_in:
    seq:
      - id: previous_output
        type: out_point
        doc: Reference to the previous transaction's output.

      - id: script_length
        type: vlq_base128_le
        doc: Length of the signature script.

      - id: script_sig
        type: b
        size: script_length.value
        doc: Computational Script for confirming transaction authorization.

      - id: sequence
        type: u4
        doc: Originally intended for "high-frequency trades", with time lock.

  tx_out:
    seq:
      - id: value
        type: u8
        doc: Number of Satoshis to be transferred.

      - id: pk_script_length
        type: vlq_base128_le
        doc: Length of the public key script.

      - id: pk_script
        type: b
        size: pk_script_length.value
        doc: Usually contains the public key as a Bitcoin script setting up conditions to claim this output.

  out_point:
    seq:
      - id: hash
        type: b
        size: 32
        doc: The hash of the referenced transaction.

      - id: index
        type: u4
        doc: The index of the specific output in the transaction. The first output is 0, etc.

  vlq_base128_le:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: _.value & 0x80 == 0
        doc: Base128 encoded variable length integer, little endian.

instances:
  tx_in_count:
    value: tx_in_count.groups.map(lambda x: x.value & 0x7F).fold(0, |acc, x| acc * 128 + x)
  tx_out_count:
    value: tx_out_count.groups.map(lambda x: x.value & 0x7F).fold(0, |acc, x| acc * 128 + x)