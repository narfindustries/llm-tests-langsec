type: struct
id: ntpv4
fields:
  - id: li
    type: bits
    size: 1
  - id: vn
    type: bits
    size: 3
  - id: mode
    type: bits
    size: 3
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: f4
  - id: root_dispersion
    type: f4
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
