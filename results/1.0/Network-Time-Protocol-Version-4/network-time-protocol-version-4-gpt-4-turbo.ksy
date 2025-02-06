meta:
  id: ntp_packet
  title: Network Time Protocol (NTP) Version 4
  endian: be
seq:
  - id: li
    type: b2
  - id: vn
    type: b3
  - id: mode
    type: b3
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: fixed_point
  - id: root_dispersion
    type: fixed_point
  - id: reference_id
    type: u4
  - id: ref_timestamp
    type: ntp_timestamp
  - id: org_timestamp
    type: ntp_timestamp
  - id: rec_timestamp
    type: ntp_timestamp
  - id: xmt_timestamp
    type: ntp_timestamp
  - id: extensions
    type: extension
    repeat: eos
types:
  fixed_point:
    seq:
      - id: int_part
        type: u2
      - id: fraction_part
        type: u2
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
  extension:
    seq:
      - id: extension_type
        type: u2
      - id: extension_len
        type: u2
      - id: extension_data
        type: u1
        repeat: expr
        repeat-expr: extension_len