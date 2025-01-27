meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  file-extension: btc
  endian: le
  license: CC0-1.0
doc: |
  A Bitcoin transaction structure, parsed as per Bitcoin protocol.

seq:
  - id: version
    type: u4
    doc: Transaction version number.

  - id: tx_in_count
    type: vlq_base128_le
    doc: Number of transaction inputs.

  - id: tx_ins
    type: tx_in
    repeat: expr
    repeat-expr: tx_in_count.value
    doc: List of transaction inputs.

  - id: tx_out_count
    type: vlq_base128_le
    doc: Number of transaction outputs.

  - id: tx_outs
    type: tx_out
    repeat: expr
    repeat-expr: tx_out_count.value
    doc: List of transaction outputs.

  - id: lock_time
    type: u4
    doc: A time lock until which transaction is locked.

types:
  tx_in:
    seq:
      - id: prev_tx
        type: u4
        doc: Hash of the previous transaction.
      
      - id: prev_index
        type: u4
        doc: Index of the output in the previous transaction.

      - id: script_len
        type: vlq_base128_le
        doc: Length of the script.

      - id: script_sig
        type: b
        size: script_len.value
        doc: Script that provides data needed to satisfy spending conditions.

      - id: seq_no
        type: u4
        doc: Sequence number, default for Bitcoin is 0xFFFFFFFF.

  tx_out:
    seq:
      - id: value
        type: u8
        doc: Number of Satoshis to be transferred.

      - id: script_len
        type: vlq_base128_le
        doc: Length of the script.

      - id: script_pub_key
        type: b
        size: script_len.value
        doc: Script that provides conditions under which the output can be spent.