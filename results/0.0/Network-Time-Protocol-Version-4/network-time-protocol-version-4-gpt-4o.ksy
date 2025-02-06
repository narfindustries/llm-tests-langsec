meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  application: network
  file-extension: ntp
  endian: be

seq:
  - id: li_vn_mode
    type: u1
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
    type: authenticator
    if: _io.size - _io.pos >= 12

types:
  ntp_timestamp:
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
        type: u8

instances:
  li:
    value: (li_vn_mode >> 6) & 0x3
  vn:
    value: (li_vn_mode >> 3) & 0x7
  mode:
    value: li_vn_mode & 0x7