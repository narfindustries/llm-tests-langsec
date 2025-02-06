meta:
  id: ntp_v4
  title: Network Time Protocol Version 4 (NTPv4)
  file-extension: ntp
  license: MIT
  endian: be
seq:
  - id: leap_indicator
    type: b2
    doc: Leap Indicator (LI), 2 bits.
  - id: version_number
    type: b3
    doc: Version Number (VN), 3 bits.
  - id: mode
    type: b3
    doc: Mode, 3 bits.
  - id: stratum
    type: u1
    doc: Stratum, 8 bits.
  - id: poll
    type: s1
    doc: Poll interval, 8-bit signed integer.
  - id: precision
    type: s1
    doc: Clock precision, 8-bit signed integer.
  - id: root_delay
    type: f4
    doc: Root Delay, 32-bit fixed-point number.
  - id: root_dispersion
    type: f4
    doc: Root Dispersion, 32-bit fixed-point number.
  - id: reference_id
    type: u4
    doc: Reference ID, 32 bits.
  - id: reference_timestamp
    type: u8
    doc: Reference Timestamp, 64-bit NTP timestamp.
  - id: origin_timestamp
    type: u8
    doc: Origin Timestamp, 64-bit NTP timestamp.
  - id: receive_timestamp
    type: u8
    doc: Receive Timestamp, 64-bit NTP timestamp.
  - id: transmit_timestamp
    type: u8
    doc: Transmit Timestamp, 64-bit NTP timestamp.
  - id: key_identifier
    type: u4
    doc: Key Identifier, 32 bits (optional).
  - id: message_digest
    size: 16
    doc: Message Digest, 128 bits (optional).
  - id: extensions
    type: extension_field
    repeat: eos
    doc: Optional extension fields.
types:
  extension_field:
    seq:
      - id: extension_field_type
        type: u2
        doc: Extension Field Type, 16 bits.
      - id: extension_field_length
        type: u2
        doc: Extension Field Length, 16 bits.
      - id: extension_field_value
        size: extension_field_length
        doc: Extension Field Value, variable length.
  f4:
    doc: 32-bit fixed-point number.
    seq:
      - id: value
        type: s4
  b2:
    doc: 2-bit unsigned integer.
    seq:
      - id: value
        type: u1
  b3:
    doc: 3-bit unsigned integer.
    seq:
      - id: value
        type: u1