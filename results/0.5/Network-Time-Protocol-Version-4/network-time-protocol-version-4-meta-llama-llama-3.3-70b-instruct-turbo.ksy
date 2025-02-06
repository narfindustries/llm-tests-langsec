meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be
seq:
  - id: leap_indicator
    type: u2
  - id: version_number
    type: u3
  - id: mode
    type: u3
  - id: poll
    type: u8
  - id: precision
    type: u8
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: ref_id
    type: u4
  - id: ref_timestamp
    type: u8
  - id: origin_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
  - id: extensions
    type: extension_field
    repeat: 
      expr: extension.type != 0
types:
  extension_field:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: data
        type: bytes
        size: length
      - id: padding
        type: bytes
        size: (4 - (length % 4)) % 4