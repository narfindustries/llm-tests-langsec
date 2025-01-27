meta:
  id: ntp_packet
  title: NTP v4 Packet
  file-extension: ntp
  endian: be

seq:
  - id: li_vn_mode
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: s1
  - id: root_delay
    type: s4
  - id: root_dispersion
    type: u4
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
    repeat: eos
    if: _io.pos < _io.size

types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
      - id: fraction
        type: u4

  extension_field:
    seq:
      - id: type
        type: u2
      - id: length
        type: u2
      - id: value
        size: length - 4

instances:
  leap_indicator:
    value: (li_vn_mode >> 6) & 0x3
  version:
    value: (li_vn_mode >> 3) & 0x7
  mode:
    value: li_vn_mode & 0x7

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

  stratum_types:
    0: unspecified_or_invalid
    1: primary_reference
    16: secondary_reference