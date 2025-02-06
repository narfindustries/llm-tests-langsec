meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be

seq:
  - id: header
    type: ntp_header

types:
  ntp_header:
    seq:
      - id: flags
        type: flags
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
      - id: reference_identifier
        type: reference_id
      - id: reference_timestamp
        type: timestamp
      - id: origin_timestamp
        type: timestamp
      - id: receive_timestamp
        type: timestamp
      - id: transmit_timestamp
        type: timestamp
      - id: authenticator
        type: authenticator
        if: flags.mode >= 6

  flags:
    seq:
      - id: leap_indicator
        type: b2
      - id: version
        type: b3
      - id: mode
        type: b3

  reference_id:
    seq:
      - id: data
        type:
          switch-on: _parent.stratum
          cases:
            0: str4
            1: str4
            2..255: u4

  str4:
    seq:
      - id: value
        type: str
        size: 4
        encoding: ascii

  timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

  authenticator:
    seq:
      - id: key_identifier
        type: u4
      - id: message_digest
        type: u4
      - id: padding
        type: u4
        repeat: expr
        repeat-expr: _parent.flags.mode