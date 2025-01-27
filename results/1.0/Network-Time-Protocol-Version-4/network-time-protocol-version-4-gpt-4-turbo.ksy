meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol (Version 4)
  file-extension: ntp
  endian: be
  license: GPL-2.0-only
doc: |
  The Network Time Protocol (NTP) is a protocol used to synchronize computer clock times in a network.
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
    type: u4
  - id: root_dispersion
    type: u4
  - id: ref_id
    type: u4
  - id: ref_timestamp
    type: ntp_timestamp
  - id: orig_timestamp
    type: ntp_timestamp
  - id: rx_timestamp
    type: ntp_timestamp
  - id: tx_timestamp
    type: ntp_timestamp
types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4
enums:
  leap_indicator:
    0: no_warning
    1: last_minute_61_seconds
    2: last_minute_59_seconds
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