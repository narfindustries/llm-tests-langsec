type: seq
- id: version
  type: u4
- id: inputs
  type: seq
    - type: struct
      - id: prev_out
        type: struct
          - id: hash
            type: bytes
            size: 32
          - id: index
            type: u4le
      - id: scriptSig
        type: bytes
      - id: sequence
        type: u4le
- id: outputs
  type: seq
    - type: struct
      - id: value
        type: u8le
      - id: scriptPubKey
        type: bytes
- id: locktime
  type: u4le
- id: witness
  type: seq
    - type: seq
      - type: bytes

