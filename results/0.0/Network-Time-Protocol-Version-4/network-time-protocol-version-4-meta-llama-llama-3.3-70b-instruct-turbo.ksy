meta:
  endian: be

seq:
  - id: leap
    type: u2
  - id: version
    type: u2
  - id: mode
    type: u2
  - id: poll
    type: u1
  - id: precision
    type: u1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: u8
  - id: origin_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
  - id: extensions
    type: extension
    repeat: expr
    repeat-expr: _io.pos < _io.size
    until: extension.type == 4

types:
  extension:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: body
        type:
          switch-on: type
          cases:
            - 2:
                seq:
                  - id: key_id
                    type: u4
                  - id: value
                    type: str
                    size: length - 4
            - else:
                type: bytes
                size: length - 4