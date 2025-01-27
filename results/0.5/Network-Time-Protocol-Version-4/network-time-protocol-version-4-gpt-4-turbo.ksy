meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol (Version 4)
  application: Network Time Protocol
  file-extension: ntp
  endian: be
  license: CC0-1.0

doc: |
  The Network Time Protocol (NTP) is a networking protocol for clock synchronization between computer systems over packet-switched, variable-latency data networks.

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
    type: fixed_point
  - id: root_dispersion
    type: fixed_point
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
  - id: extension_fields
    type: extension_field
    repeat: eos

types:
  fixed_point:
    seq:
      - id: integer_part
        type: u2
      - id: fraction_part
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