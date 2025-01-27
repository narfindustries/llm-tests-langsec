meta:
  id: network_time_protocol_v4
  title: Network Time Protocol Version 4
  application: network
  file-extension: ntp
  endian: big

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
    type: u4
  - id: root_dispersion
    type: u4
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
      - id: li
        type: b2
      - id: vn
        type: b3
      - id: mode
        type: b3

  timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
    instances:
      time:
        value: (seconds - 2208988800) + fraction / 4294967296.0