type: struct
id: bitcoin_transaction
endian: be
fields:
  - id: version
    type: u4
  - id: inputs
    type: seq
    repeat: eos
    fields:
      - id: prev_out
        type: struct
        fields:
          - id: hash
            type: bytes
            size: 32
          - id: index
            type: u4
      - id: scriptSig
        type: bytes
      - id: sequence
        type: u4
  - id: outputs
    type: seq
    repeat: eos
    fields:
      - id: value
        type: u8
      - id: scriptPubKey
        type: bytes
  - id: locktime
    type: u4
