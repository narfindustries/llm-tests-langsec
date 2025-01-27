meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  application: Network Time Protocol
  file-extension: ntp
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  Network Time Protocol (NTP) is a protocol used to synchronize computer clock times in a network.
  It uses a hierarchical, semi-layered system of time sources. Version 4 of the protocol is widely used
  and includes both IPv4 and IPv6 implementations.

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
  - id: extension_data
    size-eos: true

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

types:
  u4:
    seq:
      - id: value
        type: u4be

  u8:
    seq:
      - id: value
        type: u8be