meta:
  id: ntp_packet
  title: Network Time Protocol v4
  file-extension: ntp
  endian: be
seq:
  - id: li_vn_mode
    type: u1
    doc: Leap Indicator, Version Number, and Mode
  - id: stratum
    type: u1
    doc: Stratum level of the local clock
  - id: poll
    type: s1
    doc: Maximum interval between successive messages
  - id: precision
    type: s1
    doc: Precision of the local clock
  - id: root_delay
    type: s4
    doc: Total round trip delay to reference clock
  - id: root_dispersion
    type: u4
    doc: Total dispersion to reference clock
  - id: ref_id
    type: u4
    doc: Reference ID
  - id: ref_timestamp
    type: timestamp_64
    doc: Reference timestamp
  - id: origin_timestamp
    type: timestamp_64
    doc: Origin timestamp
  - id: receive_timestamp
    type: timestamp_64
    doc: Receive timestamp
  - id: transmit_timestamp
    type: timestamp_64
    doc: Transmit timestamp
  - id: extension_fields
    type: extension_field
    repeat: eos
    if: _io.eof == false
types:
  timestamp_64:
    seq:
      - id: seconds
        type: u4
        doc: Seconds since January 1, 1900
      - id: fraction
        type: u4
        doc: Fraction of a second
  extension_field:
    seq:
      - id: type
        type: u2
        doc: Extension field type
      - id: length
        type: u2
        doc: Extension field length
      - id: value
        size: length - 4
        doc: Extension field value
instances:
  leap_indicator:
    value: (li_vn_mode >> 6) & 0x3
    doc: Leap Indicator (LI)
  version_number:
    value: (li_vn_mode >> 3) & 0x7
    doc: Version Number (VN)
  mode:
    value: li_vn_mode & 0x7
    doc: Mode
enums:
  leap_indicators:
    0: no_warning
    1: last_minute_61
    2: last_minute_59
    3: alarm
  modes:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control
    7: private