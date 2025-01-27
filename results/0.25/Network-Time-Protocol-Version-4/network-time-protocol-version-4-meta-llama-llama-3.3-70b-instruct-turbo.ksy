meta:
  id: network-time-protocol-version-4
  title: Network Time Protocol Version 4
  license: MIT
  doc: |
    Network Time Protocol (NTP) is a networking protocol for clock
    synchronization between computer systems.

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
    type: f8

  - id: originate_timestamp
    type: f8

  - id: receive_timestamp
    type: f8

  - id: transmit_timestamp
    type: f8

  - id: extension
    type: extension
    repeat: expr => flags & (1 << 7) != 0

types:
  extension:
    seq:
      - id: type
        type: u2

      - id: length
        type: u2

      - id: value
        type: str
        size: length

    if: type == 0
    then:
      - id: sntp_version
        type: u1