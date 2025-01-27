meta:
  id: ntp_packet_v4
  title: Network Time Protocol Version 4
  file-extension: ntp
  endian: be
  license: MIT

seq:
  - id: li_vn_mode
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: ntp_timestamp
  - id: origin_timestamp
    type: ntp_timestamp
  - id: receive_timestamp
    type: ntp_timestamp
  - id: transmit_timestamp
    type: ntp_timestamp

types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

instances:
  leap_indicator:
    value: (li_vn_mode >> 6) & 0x3
  version:
    value: (li_vn_mode >> 3) & 0x7
  mode:
    value: li_vn_mode & 0x7

enums:
  leap_indicator_enum:
    0: no_warning
    1: last_minute_has_61_seconds
    2: last_minute_has_59_seconds
    3: alarm_condition

  mode_enum:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control_message
    7: private

  stratum_enum:
    0: unspecified_or_invalid
    1: primary_reference
    2: secondary_reference
    16: unsynchronized