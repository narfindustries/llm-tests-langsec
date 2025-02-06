meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  file-extension: ntp
  endian: be

seq:
  - id: header
    type: header
  - id: extension_fields
    type: extension_field
    repeat: eos
    if: header.mode != 0

types:
  header:
    seq:
      - id: flags
        type: u1
        doc: |
          Bit field containing Leap Indicator, Version Number, and Mode
      - id: stratum
        type: u1
        doc: Stratum level of clock
      - id: poll
        type: s1
        doc: Log2 of polling interval in seconds
      - id: precision
        type: s1
        doc: Log2 of system clock precision
      - id: root_delay
        type: u4
        doc: Total round-trip delay to primary reference
      - id: root_dispersion
        type: u4
        doc: Maximum error of system clock
      - id: reference_id
        type: str
        size: 4
        encoding: ASCII
        doc: Reference identifier
      - id: reference_timestamp
        type: timestamp
        doc: Time when system was last synchronized
      - id: origin_timestamp
        type: timestamp
        doc: Time request was sent by client
      - id: receive_timestamp
        type: timestamp
        doc: Time request was received by server
      - id: transmit_timestamp
        type: timestamp
        doc: Time reply was transmitted by server

    instances:
      leap_indicator:
        value: (flags & 0xC0) >> 6
        doc: |
          0: No warning
          1: Last minute has 61 seconds
          2: Last minute has 59 seconds
          3: Alarm (clock not synchronized)
      version_number:
        value: (flags & 0x38) >> 3
        doc: NTP version number
      mode:
        value: flags & 0x07
        doc: |
          0: Reserved
          1: Symmetric active
          2: Symmetric passive
          3: Client
          4: Server
          5: Broadcast
          6: NTP control message
          7: Private use

  timestamp:
    seq:
      - id: seconds
        type: u4
        doc: Seconds since 1900
      - id: fraction
        type: u4
        doc: Fractional seconds

  extension_field:
    seq:
      - id: field_type
        type: u2
        doc: Type of extension field
      - id: field_length
        type: u2
        doc: Length of extension field
      - id: field_value
        type: byte_array
        size: field_length
        doc: Extension field data

  byte_array:
    seq:
      - id: data
        type: u1
        repeat: expr
        repeat-expr: _parent.field_length