meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  application: Bitcoin
  file-extension: btc
  endian: le
  license: CC0-1.0
doc: |
  A Bitcoin transaction, the fundamental building block of the Bitcoin blockchain.

seq:
  - id: version
    type: u4
    doc: Transaction version number.

  - id: in_count
    type: vlq_base128_le
    doc: Number of input transactions.

  - id: inputs
    type: input
    repeat: expr
    repeat-expr: in_count
    doc: List of inputs.

  - id: out_count
    type: vlq_base128_le
    doc: Number of output transactions.

  - id: outputs
    type: output
    repeat: expr
    repeat-expr: out_count
    doc: List of outputs.

  - id: lock_time
    type: u4
    doc: The block number or timestamp at which this transaction is locked.

types:
  input:
    seq:
      - id: previous_output
        type: out_point
        doc: Reference to the output of a previous transaction.

      - id: script_len
        type: vlq_base128_le
        doc: Length of the scriptSig.

      - id: script_sig
        type: b
        size: script_len
        doc: Computational Script for confirming transaction authorization.

      - id: sequence
        type: u4
        doc: Transaction version as defined by the sender.

  output:
    seq:
      - id: value
        type: u8
        doc: Number of satoshis to be transferred.

      - id: script_len
        type: vlq_base128_le
        doc: Length of the scriptPubKey.

      - id: script_pub_key
        type: b
        size: script_len
        doc: Usually contains the public key as a Bitcoin script setting up conditions to claim this output.

  out_point:
    seq:
      - id: hash
        type: b
        size: 32
        doc: The hash of the referenced transaction.

      - id: index
        type: u4
        doc: The index of the specific output in the transaction—the first output is 0, etc.

  vlq_base128_le:
    seq:
      - id: groups
        type: u1
        repeat: until
        repeat-until: _.val & 0x80 == 0
        doc: Base128 encoded variable length integer, little endian.
    instances:
      value:
        value: |
          _.groups.map((g, i) => (g & 0x7f) << (7 * i)).reduce((a, b) => a + b, 0)
        doc: The actual value of the VLQ encoded integer.