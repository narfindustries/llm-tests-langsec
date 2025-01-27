meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  application: Network Time Protocol
  file-extension: ntp
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  Specification for the Network Time Protocol (NTP), which is used to synchronize
  the time on computers over a network. This spec covers version 4 of the protocol.

seq:
  - id: leap_indicator
    type: b2
    enum: leap_indicator
  - id: version
    type: b3
  - id: mode
    type: b3
    enum: mode
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: ntp_short
  - id: root_dispersion
    type: ntp_short
  - id: reference_id
    size: 4
  - id: reference_timestamp
    type: timestamp
  - id: originate_timestamp
    type: timestamp
  - id: receive_timestamp
    type: timestamp
  - id: transmit_timestamp
    type: timestamp
  - id: extension
    type: extension_field
    repeat: eos

types:
  ntp_short:
    seq:
      - id: seconds
        type: s2
      - id: fraction
        type: u2

  timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

  extension_field:
    seq:
      - id: field_type
        type: u2
      - id: length
        type: u2
      - id: data
        size: length

enums:
  leap_indicator:
    0: no_warning
    1: last_minute_has_61_seconds
    2: last_minute_has_59_seconds
    3: alarm_condition

  mode:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: reserved_for_ntp_control_message
    7: reserved_for_private_use