meta:
  id: bitcoin_transactions
  title: Bitcoin Transactions Structure
  file-extension: 
    - tx
    - dat
  license: MIT
  encoding: utf-8
  endian: le

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
    doc: List of transaction inputs

  - id: output_count
    type: vlq_base128_le
    doc: Number of transaction outputs

  - id: outputs
    type: transaction_output
    repeat: expr
    repeat-expr: output_count.value
    doc: List of transaction outputs

  - id: locktime
    type: u4
    doc: Transaction locktime

types:
  transaction_input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Previous transaction hash

      - id: prev_output_index
        type: u4
        doc: Index of previous output

      - id: script_length
        type: vlq_base128_le
        doc: Length of scriptSig

      - id: scriptSig
        size: script_length.value
        doc: Input script

      - id: sequence
        type: u4
        doc: Sequence number

  transaction_output:
    seq:
      - id: value
        type: u8
        doc: Amount of satoshis

      - id: script_length
        type: vlq_base128_le
        doc: Length of scriptPubKey

      - id: scriptPubKey
        size: script_length.value
        doc: Output script

  vlq_base128_le:
    seq:
      - id: groups
        type: vlq_group
        repeat: until
        repeat-until: not _.has_next
    
    instances:
      value:
        value: >-
          groups.size() > 0 ? 
          (groups.last().group & 0x7f) | 
          (groups.size() > 1 ? 
            (groups[0].group & 0x7f) << 7 : 0) | 
          (groups.size() > 2 ? 
            (groups[1].group & 0x7f) << 14 : 0) : 0

  vlq_group:
    seq:
      - id: group
        type: u1
    
    instances:
      has_next:
        value: (group & 0x80) != 0