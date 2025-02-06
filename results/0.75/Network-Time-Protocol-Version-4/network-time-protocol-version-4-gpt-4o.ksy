meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  application: network/time
  endian: be
seq:
  - id: flags
    type: flags_type
  - id: stratum
    type: u1
  - id: poll
    type: s1
  - id: precision
    type: s1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: ntp_timestamp
  - id: originate_timestamp
    type: ntp_timestamp
  - id: receive_timestamp
    type: ntp_timestamp
  - id: transmit_timestamp
    type: ntp_timestamp
  - id: authenticator
    type: authenticator_type
    if: _io.size - _io.pos >= 12
types:
  flags_type:
    seq:
      - id: leap_indicator
        type: b2
      - id: version_number
        type: b3
      - id: mode
        type: b3
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
  authenticator_type:
    seq:
      - id: key_identifier
        type: u4
      - id: message_digest
        type: u8