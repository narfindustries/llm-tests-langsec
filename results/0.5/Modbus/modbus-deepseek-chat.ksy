meta:
  id: modbus
  endian: big
  license: MIT
  ks-version: 0.9
  imports:
    - /common/bytes_with_io

seq:
  - id: transaction_id
    type: u2
  - id: protocol_id
    type: u2
  - id: length
    type: u2
  - id: unit_id
    type: u1
  - id: function_code
    type: u1
  - id: data
    size: length - 2
    type: bytes_with_io

types:
  bytes_with_io:
    seq:
      - id: data
        size-eos: true
        type: str
        encoding: ASCII