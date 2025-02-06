meta:
  id: bitcoin_transaction
  endian: le
seq:
  - id: version
    type: u4
    doc: Transaction version number
  
  - id: num_inputs
    type: u4
    doc: Number of transaction inputs
  
  - id: inputs
    type: transaction_input
    repeat: expr
    repeat-expr: num_inputs
    doc: List of transaction inputs
  
  - id: num_outputs
    type: u4
    doc: Number of transaction outputs
  
  - id: outputs
    type: transaction_output
    repeat: expr
    repeat-expr: num_outputs
    doc: List of transaction outputs
  
  - id: locktime
    type: u4
    doc: Earliest time transaction can be added to blockchain

types:
  transaction_input:
    seq:
      - id: prev_tx_hash
        size: 32
        doc: Hash of previous transaction
      
      - id: prev_tx_output_index
        type: u4
        doc: Index of output in previous transaction
      
      - id: len_script_sig
        type: u4
        doc: Length of scriptSig
      
      - id: script_sig
        size: len_script_sig
        doc: Cryptographic signature and public key
      
      - id: sequence_number
        type: u4
        doc: Sequence number for input

  transaction_output:
    seq:
      - id: amount
        type: u8
        doc: Amount of satoshis
      
      - id: len_script_pubkey
        type: u4
        doc: Length of scriptPubKey
      
      - id: script_pubkey
        size: len_script_pubkey
        doc: Destination bitcoin address or script