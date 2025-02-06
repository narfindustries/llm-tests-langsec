meta:
  id: ntp_packet
  title: Network Time Protocol v4
  file-extension: ntp
  endian: be
  license: CC0-1.0
  ks-version: 0.9

seq:
  - id: li_vn_mode
    type: u1
    doc: Combined field for Leap Indicator, Version Number and Mode
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: ntp_short_format
  - id: root_dispersion
    type: ntp_short_format
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
  - id: extension_fields
    type: extension_field
    repeat: eos
    if: _io.pos < _io.size
  - id: mac
    type: message_auth_code
    if: _io.pos < _io.size

types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

  ntp_short_format:
    seq:
      - id: seconds
        type: u2
      - id: fraction
        type: u2

  extension_field:
    seq:
      - id: field_type
        type: u2
      - id: length
        type: u2
      - id: value
        size: length - 4
      - id: padding
        size: (4 - (_io.pos % 4)) % 4
        if: _io.pos % 4 != 0

  message_auth_code:
    seq:
      - id: key_id
        type: u4
      - id: digest
        size: _io.size - _io.pos

instances:
  leap_indicator:
    value: (li_vn_mode >> 6) & 0x3
    enum: leap_indicator_enum
  version:
    value: (li_vn_mode >> 3) & 0x7
  mode:
    value: li_vn_mode & 0x7
    enum: mode_enum

enums:
  leap_indicator_enum:
    0: no_warning
    1: last_minute_61_sec
    2: last_minute_59_sec
    3: alarm_condition

  mode_enum:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: control
    7: private