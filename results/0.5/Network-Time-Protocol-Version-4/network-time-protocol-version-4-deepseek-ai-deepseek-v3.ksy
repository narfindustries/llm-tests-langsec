meta:
  id: ntp
  title: Network Time Protocol Version 4
  license: MIT
  endian: be
seq:
  - id: leap_indicator
    type: b2
    enum: leap_indicator
  - id: version_number
    type: b3
    enum: version_number
  - id: mode
    type: b3
    enum: mode
  - id: stratum
    type: u1
    enum: stratum
  - id: poll
    type: s1
  - id: precision
    type: s1
  - id: root_delay
    type: ntp_short
  - id: root_dispersion
    type: ntp_short
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
    repeat: until
    repeat-until: _.length == 0
  - id: key_identifier
    type: u4
    if: _root.mode == mode::control
  - id: digest
    type: bytes
    size: 16
    if: _root.mode == mode::control
types:
  ntp_short:
    seq:
      - id: value
        type: u4
  ntp_timestamp:
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
      - id: value
        type: bytes
        size: length
enums:
  leap_indicator:
    0: no_warning
    1: last_minute_61
    2: last_minute_59
    3: unknown
  version_number:
    4: ntpv4
  mode:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: control
    7: private
  stratum:
    0: unspecified
    1: primary
    2: secondary_2
    3: secondary_3
    4: secondary_4
    5: secondary_5
    6: secondary_6
    7: secondary_7
    8: secondary_8
    9: secondary_9
    10: secondary_10
    11: secondary_11
    12: secondary_12
    13: secondary_13
    14: secondary_14
    15: secondary_15
    16: unsynchronized
    17: reserved_low
    255: reserved_high