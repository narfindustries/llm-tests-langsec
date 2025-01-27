meta:
  id: network-time-protocol-version-4
  title: Network Time Protocol Version 4
  doc: |
    Network Time Protocol Version 4 (NTPv4) is widely used for clock
    synchronization across the Internet. This spec describes the format of NTPv4
    packets.
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
    type: str
    size: 4
  - id: reference_timestamp
    type: u8
  - id: originate_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8