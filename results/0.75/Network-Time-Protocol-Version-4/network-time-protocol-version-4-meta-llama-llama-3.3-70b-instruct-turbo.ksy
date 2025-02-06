meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be
seq:
  - id: flags
    type: flags
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: ref_id
    type: str
    size: 4
    encoding: ascii
  - id: ref_timestamp
    type: ntp_timestamp
  - id: orig_timestamp
    type: ntp_timestamp
  - id: recv_timestamp
    type: ntp_timestamp
  - id: trans_timestamp
    type: ntp_timestamp
  - id: extension_fields
    type: extension_field
    repeat: expr
    repeat_expr: _io.pos < _io.size
types:
  u1:
    type: uint8
  u2:
    type: uint16
  u4:
    type: uint32
  s1:
    type: int8
  str:
    type: str
  flags:
    type: bits
    num: 8
    seq:
      - id: leap
        type: bit
        pos: 6
        size: 2
      - id: version
        type: bit
        pos: 3
        size: 3
      - id: mode
        type: bit
        pos: 0
        size: 3
  ntp_timestamp:
    type: struct
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
  extension_field:
    type: struct
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: value
        type: bytes
        size: length
      - id: padding
        type: bytes
        size: 4 - (length % 4)