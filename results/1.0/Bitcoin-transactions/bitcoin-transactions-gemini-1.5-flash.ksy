types:
  uint32:
    seq:
      - id: value
        type: u4le
  uint64:
    seq:
      - id: value
        type: u8le
  varint:
    seq:
      - id: value
        type: cb
  bytes32:
    seq:
      - id: value
        type: u1
        repeat: 32
  tx_in:
    seq:
      - id: previous_output
        type: bytes32
      - id: scriptSig
        type: varint
      - id: sequence
        type: uint32
  tx_out:
    seq:
      - id: value
        type: uint64
      - id: scriptPubKey
        type: varint
  tx:
    seq:
      - id: version
        type: uint32
      - id: in
        type: tx_in
        repeat: eos
        size: (varint)
      - id: out
        type: tx_out
        repeat: eos
        size: (varint)
      - id: locktime
        type: uint32
