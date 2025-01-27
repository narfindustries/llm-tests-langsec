meta:
  id: network-time-protocol-version-4
  title: Network Time Protocol Version 4
  file-format: network-protocol
  endian: big
seq:
  - id: flags
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: u1
  - id: delay
    type: u4
  - id: dispersion
    type: u4
  - id: identifier
    type: u4
  - id: reference_timestamp
    type: f8
  - id: originate_timestamp
    type: f8
  - id: receive_timestamp
    type: f8
  - id: transmit_timestamp
    type: f8