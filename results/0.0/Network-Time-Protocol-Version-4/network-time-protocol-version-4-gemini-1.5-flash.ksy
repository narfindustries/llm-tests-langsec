meta:
  id: network-time-protocol-version-4-gemini-1
  title: Network Time Protocol Version 4 (Gemini 1)
  homepage: https://www.ietf.org/rfc/rfc1305.txt
  file-extension: bin
  endian: be

seq:
  - id: leap
    type: u1
  - id: version
    type: u1
    enum:
      0: unspecified
      1: ntp-version-1
      2: ntp-version-2
      3: ntp-version-3
      4: ntp-version-4
  - id: mode
    type: u1
    enum:
      0: reserved
      1: symmetric-active
      2: symmetric-passive
      3: client
      4: server
      5: broadcast
      6: ntp-control-message
      7: private
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: u1
  - id: root_delay
    type: s8
  - id: root_dispersion
    type: u8
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: timestamp
  - id: origin_timestamp
    type: timestamp
  - id: receive_timestamp
    type: timestamp
  - id: transmit_timestamp
    type: timestamp

types:
  timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

