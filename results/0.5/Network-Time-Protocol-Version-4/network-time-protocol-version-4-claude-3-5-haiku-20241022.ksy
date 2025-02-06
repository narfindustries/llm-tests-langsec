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
        doc: Log2 of maximum interval between messages
      - id: precision
        type: s1
        doc: Log2 of system clock precision
      - id: root_delay
        type: u4
        doc: Total round-trip delay to primary reference source
      - id: root_dispersion
        type: u4
        doc: Total dispersion to primary reference source
      - id: reference_id
        type: u4
        doc: Reference identifier
      - id: reference_timestamp
        type: timestamp
        doc: Time of last calibration
      - id: originate_timestamp
        type: timestamp
        doc: Time request sent by client
      - id: receive_timestamp
        type: timestamp
        doc: Time request received by server
      - id: transmit_timestamp
        type: timestamp
        doc: Time reply sent by server
      - id: authentication
        type: authentication
        if: _io.size - _io.pos >= 20
        doc: Optional authentication fields

  timestamp:
    seq:
      - id: seconds
        type: u4
        doc: Seconds since January 1, 1900
      - id: fraction
        type: u4
        doc: Fractional seconds

  authentication:
    seq:
      - id: key_identifier
        type: u4
        doc: Key identifier for authentication
      - id: message_digest
        size: 16
        doc: MD5 message digest

instances:
  leap_indicator:
    value: header.flags >> 6
    enum: leap_type
  version:
    value: (header.flags >> 3) & 0b111
  mode:
    value: header.flags & 0b111
    enum: mode_type

enums:
  leap_type:
    0: no_warning
    1: last_minute_61_seconds
    2: last_minute_59_seconds
    3: alarm_condition

  mode_type:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control
    7: private_use