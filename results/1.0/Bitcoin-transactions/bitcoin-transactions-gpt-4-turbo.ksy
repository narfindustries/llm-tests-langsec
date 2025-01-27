meta:
  id: bitcoin_transaction
  title: Bitcoin Transaction
  application: Bitcoin
  file-extension: btc
  endian: le
  license: CC0-1.0

doc: |
  A Bitcoin transaction tells the network that the owner of some bitcoin value has
  authorized the transfer of that value to another owner. The new owner can now spend
  the bitcoin by creating another transaction that authorizes transfer to another owner,
  and so on, in a chain of ownership.

seq:
  - id: version
    type: s4
  - id: tx_in_count
    type: u1
  - id: tx_in
    type: tx_in
    repeat: expr
    repeat-expr: tx_in_count
  - id: tx_out_count
    type: u1
  - id: tx_out
    type: tx_out
    repeat: expr
    repeat-expr: tx_out_count
  - id: lock_time
    type: u4

types:
  tx_in:
    seq:
      - id: previous_output
        type: outpoint
      - id: script_length
        type: vlq_base128_le
      - id: script_sig
        type: b
        size: script_length.value
      - id: sequence
        type: u4

  tx_out:
    seq:
      - id: value
        type: u8
      - id: script_length
        type: vlq_base128_le
      - id: script_pub_key
        type: b
        size: script_length.value

  outpoint:
    seq:
      - id: hash
        type: b
        size: 32
      - id: index
        type: u4

  vlq_base128_le:
    doc: |
      Variable length quantity (VLQ) as used in bitcoin protocol to encode integers more efficiently when higher bits are zeros.
    seq:
      - id: groups
        type: group
        repeat: eos
    instances:
      value:
        value: >-
          (groups.isDefined) &&
          groups.reduce((acc, g) => (acc << 7) | (g.value), 0)

  group:
    seq:
      - id: b
        type: b1
      - id: value
        type: b7
        
    instances:
      more:
        value: b == 1
