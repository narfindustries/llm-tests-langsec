types:
  uint8:
    seq:
      - id: value
        type: u1
  uint32:
    seq:
      - id: value
        type: u4
  uint64:
    seq:
      - id: value
        type: u8
  varint:
    seq:
      - id: value
        type: varint
  hash256:
    seq:
      - id: value
        type: u8
        size: 32
  script:
    seq:
      - id: len
        type: varint
      - id: data
        type: u8
        size: len
  txin:
    seq:
      - id: prev_out
        type: txoutpoint
      - id: script_sig
        type: script
      - id: sequence
        type: uint32
  txout:
    seq:
      - id: value
        type: uint64
      - id: script_pubkey
        type: script
  txoutpoint:
    seq:
      - id: hash
        type: hash256
      - id: index
        type: uint32

instances:
  tx:
    seq:
      - id: version
        type: uint32
      - id: txins
        type: txin
        repeat: true
      - id: txouts
        type: txout
        repeat: true
      - id: locktime
        type: uint32
    size: 4 + (4 + script.len + 4) * txins.len + (8 + script.len) * txouts.len + 4

