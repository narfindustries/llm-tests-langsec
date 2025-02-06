seq:
  - id: transaction
    type: transaction

types:
  transaction:
    seq:
      - id: version
        type: uint32le
      - id: num_inputs
        type: varint
      - id: inputs
        type: input
        repeat: num_inputs
      - id: num_outputs
        type: varint
      - id: outputs
        type: output
        repeat: num_outputs
      - id: lock_time
        type: uint32le

  input:
    seq:
      - id: prev_txid
        type: bytes
        size: 32
      - id: prev_out_idx
        type: uint32le
      - id: script_len
        type: varint
      - id: script
        type: bytes
        size: expr
        expr: script_len.value
      - id: sequence
        type: uint32le

  output:
    seq:
      - id: value
        type: uint64le
      - id: script_len
        type: varint
      - id: script
        type: bytes
        size: expr
        expr: script_len.value

  varint:
    seq:
      - id: first_byte
        type: uint8
      - id: value
        type:
          switch-on: first_byte
          cases:
            - when: "<= 0xfd"
              type: uint8
            - when: "== 0xfd"
              type: uint16le
            - when: "== 0xfe"
              type: uint32le
            - when: "== 0xff"
              type: uint64le

  bytes:
    seq:
      - id: data
        type: uint8
        repeat: expr

  fixed_size_bytes:
    params:
      size: int
    seq:
      - id: data
        type: uint8
        repeat: expr
        expr: size

  uint32le:
    type: fixed_size_bytes
    params:
      size: 4

  uint64le:
    type: fixed_size_bytes
    params:
      size: 8

  uint16le:
    type: fixed_size_bytes
    params:
      size: 2

  uint8:
    type: fixed_size_bytes
    params:
      size: 1