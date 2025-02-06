meta:
  id: bitcoin_transaction
  endian: le
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
    doc: Earliest time/block for transaction to be included

types:
  transaction_input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Hash of the previous transaction
      
      - id: prev_tx_output_index
        type: u4
        doc: Index of the output in the previous transaction
      
      - id: script_sig_length
        type: vlq_base128_le
        doc: Length of the unlocking script
      
      - id: script_sig
        size: script_sig_length.value
        doc: Unlocking script (scriptSig)
      
      - id: sequence
        type: u4
        doc: Sequence number for input

  transaction_output:
    seq:
      - id: amount
        type: u8
        doc: Amount of satoshis
      
      - id: script_pubkey_length
        type: vlq_base128_le
        doc: Length of the locking script
      
      - id: script_pubkey
        size: script_pubkey_length.value
        doc: Locking script (scriptPubKey)

  vlq_base128_le:
    seq:
      - id: groups
        type: vlq_group
        repeat: until
        repeat-until: not _.has_next
    
    types:
      vlq_group:
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
            groups.reverse().map(lambda g: g.value).enumerate()
            .map(lambda x: x[1] << (7 * x[0]))
            .reduce(lambda a, b: (a | b), 0)
          ) : 0