meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  application: network
  license: CC0-1.0
  endian: be
seq:
  - id: flags
    type: flags
  - id: stratum
    type: u1
  - id: poll
    type: s1
  - id: precision
    type: s1
  - id: root_delay
    type: fixed_point_16
  - id: root_dispersion
    type: fixed_point_16
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: timestamp
  - id: originate_timestamp
    type: timestamp
  - id: receive_timestamp
    type: timestamp
  - id: transmit_timestamp
    type: timestamp
types:
  flags:
    seq:
      - id: leap_indicator
        type: b2
      - id: version_number
        type: b3
      - id: mode
        type: b3
  fixed_point_16:
    seq:
      - id: integer
        type: s2
      - id: fraction
        type: u2
  timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4