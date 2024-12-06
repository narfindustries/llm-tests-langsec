meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  file-extension: ntp
  endian: be
seq:
  - id: leap_indicator
    type: u1
    doc: Leap indicator field
  - id: version_number
    type: u1
    doc: Version number field
  - id: mode
    type: u1
    doc: Mode field
  - id: stratum
    type: u1
    doc: Stratum field
  - id: poll
    type: u1
    doc: Poll interval
  - id: precision
    type: u1
    doc: Precision of the clock
  - id: root_delay
    type: u4
    doc: Total round-trip delay to the reference clock
  - id: root_dispersion
    type: u4
    doc: Total dispersion to the reference clock
  - id: reference_id
    type: u4
    doc: Reference clock identifier
  - id: reference_timestamp
    type: u8
    doc: Reference timestamp
  - id: originate_timestamp
    type: u8
    doc: Originate timestamp
  - id: receive_timestamp
    type: u8
    doc: Receive timestamp
  - id: transmit_timestamp
    type: u8
    doc: Transmit timestamp