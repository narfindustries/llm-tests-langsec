meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be

seq:
  - id: flags
    type: flags
    size: 1
  - id: stratum
    type: u1
  - id: poll_interval
    type: s1
  - id: precision
    type: s1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: reference_id
    type: reference_identifier(stratum)
  - id: reference_timestamp
    type: timestamp
  - id: originate_timestamp
    type: timestamp
  - id: receive_timestamp
    type: timestamp
  - id: transmit_timestamp
    type: timestamp
  - id: authenticator
    type: authenticator
    if: _io.size - _io.pos > 0

types:
  flags:
    seq:
      - id: leap_indicator
        type: b2
      - id: version
        type: b3
      - id: mode
        type: b3

  reference_identifier:
    params:
      - id: parent_stratum
        type: u1
    seq:
      - id: value
        type:
          switch-on: parent_stratum
          cases:
            0: str_code
            1: str_code
            2..255: ipv4_address

  str_code:
    seq:
      - id: code
        type: str
        size: 4
        encoding: ASCII

  ipv4_address:
    seq:
      - id: address
        type: u4

  timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

  authenticator:
    seq:
      - id: key_id
        type: u4
      - id: digest
        type: u4
        repeat: expr
        repeat-expr: 4