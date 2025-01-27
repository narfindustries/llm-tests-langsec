meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  endian: be
  bit-endian: be

seq:
  - id: leap_indicator
    type: b2
    doc: "Leap second warning indicator"

  - id: version
    type: b3
    doc: "NTP protocol version"

  - id: mode
    type: b3
    doc: "NTP mode (client, server, etc.)"

  - id: stratum
    type: u1
    doc: "Stratum level of clock"

  - id: poll_interval
    type: s1
    doc: "Poll interval in log2 seconds"

  - id: precision
    type: s1
    doc: "Clock precision in log2 seconds"

  - id: root_delay
    type: u4
    doc: "Total round-trip delay to reference clock"

  - id: root_dispersion
    type: u4
    doc: "Maximum error of clock"

  - id: reference_identifier
    type: u4
    doc: "Reference clock identifier"

  - id: reference_timestamp
    type: u8
    doc: "Time when last calibration occurred"

  - id: originate_timestamp
    type: u8
    doc: "Time request was sent by client"

  - id: receive_timestamp
    type: u8
    doc: "Time request was received by server"

  - id: transmit_timestamp
    type: u8
    doc: "Time reply was sent by server"

  - id: authenticator
    type: u4
    doc: "Optional authentication data"
    if: _io.size - _io.pos >= 4