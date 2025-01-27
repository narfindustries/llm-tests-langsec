meta:
  id: bitcoin_transactions
  file-extension: dat
  fields:
    - name: transactions
      type: transaction
      repeat: until eof
seq:
  - id: transaction
    type: transaction
transaction:
  seq:
    - id: transaction_header
      type: transaction_header
    - id: transaction_inputs
      type: transaction_input
      repeat: varint -> transaction_header.num_inputs
    - id: transaction_outputs
      type: transaction_output
      repeat: varint -> transaction_header.num_outputs
  types:
    transaction_header:
      seq:
        - id: version
          type: u4
        - id: num_inputs
          type: varint
        - id: num_outputs
          type: varint
        - id: lock_time
          type: u4
    transaction_input:
      seq:
        - id: prev_transaction_hash
          type: bytes
          size: 32
        - id: prev_transaction_index
          type: u4
        - id: script_length
          type: varint
        - id: script
          type: bytes
          size: varint -> script_length
        - id: sequence
          type: u4
    transaction_output:
      seq:
        - id: value
          type: u8
        - id: script_length
          type: varint
        - id: script
          type: bytes
          size: varint -> script_length
    varint:
      seq:
        - id: first_byte
          type: u1
        - id: rest
          type: u1
          repeat: when -> first_byte & 0x7F == 0x7F
      types:
        value:
          if: first_byte & 0xFD == 0xFD
            then: u2 -> rest
          else-if: first_byte & 0xFE == 0xFE
            then: u4 -> rest
          else-if: first_byte & 0xFF == 0xFF
            then: u8 -> rest
          else:
            then: u1 -> first_byte