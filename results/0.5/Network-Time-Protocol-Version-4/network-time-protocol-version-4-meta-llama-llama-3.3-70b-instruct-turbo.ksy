meta:
  id: network-time-protocol-version-4
  title: Network Time Protocol Version 4
  file-format: binary
seq:
  - id: flags
    type: u2
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: i1
  - id: delay
    type: u4
  - id: dispersion
    type: u4
  - id: identifier
    type: str
    size: 4
  - id: reference_timestamp
    type: f8
  - id: originate_timestamp
    type: f8
  - id: receive_timestamp
    type: f8
  - id: transmit_timestamp
    type: f8

types:
  u2: uint16
  u1: uint8
  u4: uint32
  i1: int8
  f8: float64