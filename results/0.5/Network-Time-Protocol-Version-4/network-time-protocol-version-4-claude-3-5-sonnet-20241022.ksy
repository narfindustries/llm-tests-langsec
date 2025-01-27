meta:
  id: ntp_packet
  title: NTP v4 Packet
  file-extension: ntp
  endian: be

doc: |
  Network Time Protocol Version 4 (NTPv4) packet format specification.
  RFC 5905: https://tools.ietf.org/html/rfc5905

seq:
  - id: li_vn_mode
    type: u1
    doc: Leap Indicator (2 bits), Version Number (3 bits), Mode (3 bits)
  - id: stratum
    type: u1
    doc: Stratum level of the local clock
  - id: poll
    type: s1
    doc: Maximum interval between successive messages (log2 seconds)
  - id: precision
    type: s1
    doc: Precision of the local clock (log2 seconds)
  - id: root_delay
    type: s4
    doc: Total round-trip delay to the reference clock
  - id: root_dispersion
    type: u4
    doc: Total dispersion to the reference clock
  - id: reference_id
    type: u4
    doc: Reference ID (ASCII or IP address)
  - id: reference_timestamp
    type: timestamp_64
    doc: Time when the system clock was last set or corrected
  - id: origin_timestamp
    type: timestamp_64
    doc: Time at the client when the request departed for the server
  - id: receive_timestamp
    type: timestamp_64
    doc: Time at the server when the request arrived from the client
  - id: transmit_timestamp
    type: timestamp_64
    doc: Time at the server when the response left for the client
  - id: extension_fields
    type: extension_field
    if: _io.eof == false
    repeat: until
    repeat-until: _io.eof == true
    doc: Optional extension fields

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
      - id: field_type
        type: u2
        doc: Type of extension field
      - id: length
        type: u2
        doc: Length of entire extension field
      - id: value
        size: length - 4
        doc: Extension field value

instances:
  leap_indicator:
    value: (li_vn_mode & 0xC0) >> 6
    doc: Leap Indicator (0-3)
  version:
    value: (li_vn_mode & 0x38) >> 3
    doc: Version Number (4 for NTPv4)
  mode:
    value: li_vn_mode & 0x07
    doc: Mode (0-7)

enums:
  leap_indicators:
    0: no_warning
    1: last_minute_61
    2: last_minute_59
    3: alarm_condition

  modes:
    0: reserved
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control_message
    7: reserved_private