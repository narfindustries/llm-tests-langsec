meta:
  id: ntp_packet
  title: Network Time Protocol (NTP) Version 4
  endian: be
seq:
  - id: li_vn_mode
    type: b8
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: s4
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
  - id: extension_data
    type: u1
    repeat: expr
    repeat-expr: _io.size - _io.pos
types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
instances:
  leap_indicator:
    value: li_vn_mode >> 6
  version:
    value: (li_vn_mode & 0x38) >> 3
  mode:
    value: li_vn_mode & 0x07