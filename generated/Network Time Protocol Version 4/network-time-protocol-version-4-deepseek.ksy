meta:
  id: network_time_protocol_version_4
  endian: big
seq:
  - id: leap_indicator
    type: u1
    doc: |
      Leap Indicator (LI): This is a 2-bit code warning of an impending leap second to be inserted/deleted in the last minute of the current day.
    enum:
      0: no_warning
      1: last_minute_has_61_seconds
      2: last_minute_has_59_seconds
      3: alarm_clock_unsynchronized
  - id: version_number
    type: u1
    doc: |
      Version Number (VN): This is a 3-bit integer indicating the NTP version number.
    enum:
      4: version_4
  - id: mode
    type: u1
    doc: |
      Mode: This is a 3-bit integer indicating the mode of operation.
    enum:
      0: reserved
      1: symmetric_active
      2: symmetric_passive
      3: client
      4: server
      5: broadcast
      6: ntp_control_message
      7: reserved_for_private_use
  - id: stratum
    type: u1
    doc: |
      Stratum: This is an 8-bit unsigned integer indicating the stratum level of the local clock.
    enum:
      0: unspecified_or_invalid
      1: primary_reference
      2: secondary_reference
  - id: poll
    type: u1
    doc: |
      Poll: This is an 8-bit signed integer indicating the maximum interval between successive messages.
  - id: precision
    type: u1
    doc: |
      Precision: This is an 8-bit signed integer indicating the precision of the local clock.
  - id: root_delay
    type: u4
    doc: |
      Root Delay: This is a 32-bit signed fixed-point number indicating the total roundtrip delay to the primary reference source.
  - id: root_dispersion
    type: u4
    doc: |
      Root Dispersion: This is a 32-bit unsigned fixed-point number indicating the maximum error due to the clock frequency tolerance.
  - id: reference_identifier
    type: u4
    doc: |
      Reference Identifier: This is a 32-bit bitstring identifying the particular reference source.
  - id: reference_timestamp
    type: u8
    doc: |
      Reference Timestamp: This is the time when the system clock was last set or corrected.
  - id: originate_timestamp
    type: u8
    doc: |
      Originate Timestamp: This is the time at which the request departed the client for the server.
  - id: receive_timestamp
    type: u8
    doc: |
      Receive Timestamp: This is the time at which the request arrived at the server.
  - id: transmit_timestamp
    type: u8
    doc: |
      Transmit Timestamp: This is the time at which the reply departed the server for the client.
  - id: extension_field_1
    type: u8
    doc: |
      Extension Field 1: Optional field for additional information.
    repeat: expr
    repeat-expr: mode == 4 ? 0 : 1
  - id: extension_field_2
    type: u8
    doc: |
      Extension Field 2: Optional field for additional information.
    repeat: expr
    repeat-expr: mode == 4 ? 0 : 1
  - id: key_identifier
    type: u4
    doc: |
      Key Identifier: Optional field for cryptographic authentication.
    repeat: expr
    repeat-expr: mode == 4 ? 0 : 1
  - id: message_digest
    type: u16
    doc: |
      Message Digest: Optional field for cryptographic authentication.
    repeat: expr
    repeat-expr: mode == 4 ? 0 : 1