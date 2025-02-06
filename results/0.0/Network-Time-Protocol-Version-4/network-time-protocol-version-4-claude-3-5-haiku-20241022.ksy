meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be

seq:
  - id: header
    type: ntp_header

types:
  ntp_header:
    seq:
      - id: flags
        type: u1
        doc: Leap Indicator, Version, Mode
      - id: stratum
        type: u1
        doc: Stratum level of clock
      - id: poll_interval
        type: s1
        doc: Log2 of polling interval
      - id: precision
        type: s1
        doc: Log2 of system clock precision
      - id: root_delay
        type: u4
        doc: Total round-trip delay to primary reference
      - id: root_dispersion
        type: u4
        doc: Maximum error relative to primary reference
      - id: reference_identifier
        type: str
        size: 4
        encoding: ASCII
        doc: Reference source identifier
      - id: reference_timestamp
        type: ntp_timestamp
        doc: Time when system clock was last set
      - id: origin_timestamp
        type: ntp_timestamp
        doc: Time request sent by client
      - id: receive_timestamp
        type: ntp_timestamp
        doc: Time request arrived at server
      - id: transmit_timestamp
        type: ntp_timestamp
        doc: Time reply sent by server

  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
        doc: Seconds since Jan 1, 1900
      - id: fraction
        type: u4
        doc: Fractional seconds

instances:
  leap_indicator:
    value: header.flags >> 6
    enum: leap_indicator_type
  version:
    value: (header.flags >> 3) & 0b111
  mode:
    value: header.flags & 0b111
    enum: mode_type

enums:
  leap_indicator_type:
    0: no_warning
    1: last_minute_61_seconds
    2: last_minute_59_seconds
    3: alarm_not_synchronized

  mode_type:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control
    7: private_use