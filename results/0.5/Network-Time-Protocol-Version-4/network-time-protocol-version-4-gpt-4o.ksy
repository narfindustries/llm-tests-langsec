meta:
  id: ntp_packet
  title: Network Time Protocol Version 4
  license: CC0-1.0
  endian: be

seq:
  - id: li_vn_mode
    type: li_vn_mode
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
    type: u8
  - id: originate_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
  - id: optional_authenticator
    type: authenticator
    if: _io.size - _io.pos > 0

types:
  li_vn_mode:
    seq:
      - id: leap_indicator
        type: b2
      - id: version_number
        type: b3
      - id: mode
        type: b3

  authenticator:
    seq:
      - id: key_identifier
        type: u4
      - id: message_digest
        size: 16 # MD5 message digest size in bytes
        if: _io.size - _io.pos >= 16