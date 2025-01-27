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
  The new owner can now spend the bitcoin by creating another transaction
  that authorizes transfer to another owner, and so on, in a chain of ownership.

seq:
  - id: version
    type: u4
    doc: Transaction version number.

  - id: in_count
    type: vlq_base128_le
    doc: Number of input transactions.

  - id: inputs
    type: tx_input
    repeat: expr
    repeat-expr: in_count.value

  - id: out_count
    type: vlq_base128_le
    doc: Number of output transactions.

  - id: outputs
    type: tx_output
    repeat: expr
    repeat-expr: out_count.value

  - id: lock_time
    type: u4
    doc: A time or block number which determines when this transaction is locked.

types:
  tx_input:
    seq:
      - id: prev_transaction_hash
        size: 32
        doc: The hash of the previous transaction.

      - id: prev_tx_out_index
        type: u4
        doc: The index of the output in the previous transaction.

      - id: script_length
        type: vlq_base128_le
        doc: Length of the script.

      - id: script_sig
        size-eos: true
        type: b
        doc: Computational Script for confirming transaction authorization.

      - id: sequence
        type: u4
        doc: Sequence number. Default for Bitcoin is 0xffffffff.

  tx_output:
    seq:
      - id: value
        type: u8
        doc: Number of satoshis to transfer.

      - id: script_length
        type: vlq_base128_le
        doc: Length of the script.

      - id: script_pub_key
        size-eos: true
        type: b
        doc: Usually contains the public key as a Bitcoin script setting up conditions to claim this output.

types:
  vlq_base128_le:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: _.last_group
        doc: Base128 encoded variable length integer

    instances:
      value:
        value: 'sum([(x.groups & 0x7F) << (7 * i) for i, x in enumerate(_.groups)])'
      last_group:
        value: '(groups[-1] & 0x80) == 0'