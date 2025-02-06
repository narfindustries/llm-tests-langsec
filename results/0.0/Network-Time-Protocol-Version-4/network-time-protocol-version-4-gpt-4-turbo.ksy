meta:
  id: ntp_packet
  title: Network Time Protocol Version 4
  endian: be
  xref:
    rfc: 5905
seq:
  - id: li_vn_mode
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: ntp_short
  - id: root_dispersion
    type: ntp_short
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: ntp_timestamp
  - id: origin_timestamp
    type: ntp_timestamp
  - id: receive_timestamp
    type: ntp_timestamp
  - id: transmit_timestamp
    type: ntp_timestamp
types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
  ntp_short:
    seq:
      - id: seconds
        type: s2
      - id: fraction
        type: u2
instances:
  leap_indicator:
    value: li_vn_mode >> 6
  version_number:
    value: (li_vn_mode & 0x38) >> 3
  mode:
    value: li_vn_mode & 0x07