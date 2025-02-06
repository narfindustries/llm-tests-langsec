meta:
  id: ntp
  title: Network Time Protocol Version 4
  license: CC0-1.0
  endian: be
seq:
  - id: li
    type: b2
    doc: Leap Indicator
  - id: vn
    type: b3
    doc: Version Number
  - id: mode
    type: b3
    doc: Mode
  - id: stratum
    type: u1
    doc: Stratum
  - id: poll
    type: u1
    doc: Poll Interval
  - id: precision
    type: s1
    doc: Precision
  - id: root_delay
    type: u4
    doc: Root Delay
  - id: root_dispersion
    type: u4
    doc: Root Dispersion
  - id: ref_id
    type: u4
    doc: Reference Identifier
  - id: ref_timestamp
    type: u8
    doc: Reference Timestamp
  - id: origin_timestamp
    type: u8
    doc: Origin Timestamp
  - id: recv_timestamp
    type: u8
    doc: Receive Timestamp
  - id: transmit_timestamp
    type: u8
    doc: Transmit Timestamp
  - id: extensions
    type: extension
    repeat: eos
types:
  extension:
    seq:
      - id: length
        type: u2
      - id: data
        size: length
enums:
  leap_indicator:
    0: no_warning
    1: last_minute_61
    2: last_minute_59
    3: alarm_condition
  mode:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control_message
    7: private_use
  stratum:
    0: unspecified
    1: primary_reference
    16: unsynchronized
    valid_range:
      min: 2
      max: 15
instances:
  leap_indicator_val:
    value: li
  ntp_version_val:
    value: vn
  mode_val:
    value: mode
  stratum_val:
    value: stratum
  poll_interval_val:
    value: poll
  precision_val:
    value: precision
  root_delay_val:
    value: root_delay
  root_dispersion_val:
    value: root_dispersion
  ref_id_val:
    value: ref_id
  ref_timestamp_val:
    value: ref_timestamp
  origin_timestamp_val:
    value: origin_timestamp
  recv_timestamp_val:
    value: recv_timestamp
  transmit_timestamp_val:
    value: transmit_timestamp