meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  file-extension: ntp
  endian: be
seq:
  - id: li_vn_mode
    type: b1
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
    type: str
    size: 4
    encoding: ASCII
  - id: reference_timestamp
    type: u8
  - id: originate_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
  - id: optional_fields
    type: optional_fields
    if: _io.size > 48
types:
  optional_fields:
    seq:
      - id: key_identifier
        type: u4
      - id: message_digest
        type: bytes
        size: 16
        terminator: 0
instances:
  leap_indicator:
    value: li_vn_mode >> 6
  version_number:
    value: (li_vn_mode >> 3) & 0x7
  mode:
    value: li_vn_mode & 0x7