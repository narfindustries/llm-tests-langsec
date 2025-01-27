meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  application: Network Time Protocol
  file-extension: ntp
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  The Network Time Protocol (NTP) is a networking protocol for clock synchronization between computer systems over packet-switched, variable-latency data networks. Version 4 of the protocol is widely used and includes both accuracy and security improvements over previous versions.

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
  - id: key_identifier
    type: u4
    if: _root.stratum == 0 or _root.stratum == 1
  - id: message_digest
    size: 16
    if: _root.stratum == 0 or _root.stratum == 1

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