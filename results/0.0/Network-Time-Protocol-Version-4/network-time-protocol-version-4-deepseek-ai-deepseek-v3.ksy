meta:
  id: ntp
  title: Network Time Protocol Version 4
  license: CC0-1.0
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
    type: u4
  - id: root_dispersion
    type: u4
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: u8
  - id: originate_timestamp
    type: u8
  - id: receive_timestamp
    type: u8
  - id: transmit_timestamp
    type: u8
  - id: extension_fields
    type: u4
    repeat: until
    repeat-until: _io.eof
    if: mode == mode::ntp_control_message
  - id: key_identifier
    type: u4
    if: mode == mode::ntp_control_message
  - id: mac
    size: 16
    if: mode == mode::ntp_control_message
enums:
  leap_indicator:
    0: no_warning
    1: last_minute_61_seconds
    2: last_minute_59_seconds
    3: alarm_condition
  version_number:
    4: ntp_version_4
  mode:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control_message
    7: reserved_private_use
  stratum:
    0: unspecified_or_invalid
    1: primary_server
    2: secondary_server_2
    3: secondary_server_3
    4: secondary_server_4
    5: secondary_server_5
    6: secondary_server_6
    7: secondary_server_7
    8: secondary_server_8
    9: secondary_server_9
    10: secondary_server_10
    11: secondary_server_11
    12: secondary_server_12
    13: secondary_server_13
    14: secondary_server_14
    15: secondary_server_15
    16: reserved_16
    255: reserved_255