meta:
  id: ntp_packet
  title: Network Time Protocol (Version 4)
  license: CC0-1.0
  endian: be

doc: |
  Network Time Protocol (Version 4) to synchronize time over a network.
  
seq:
  - id: li_vn_mode
    type: u1
    doc: Leap indicator, version and mode.
  - id: stratum
    type: u1
    doc: Stratum level of the local clock.
  - id: poll
    type: u1
    doc: Maximum interval between successive messages.
  - id: precision
    type: u1
    doc: Precision of the local clock.
  - id: root_delay
    type: u4
    doc: Total round trip delay to the primary reference source.
  - id: root_dispersion
    type: u4
    doc: Total dispersion to the primary reference source.
  - id: reference_id
    type: u4
    doc: Identifier of the particular server or reference clock.
  - id: reference_timestamp
    type: ntp_timestamp
    doc: Time when the system clock was last set or corrected.
  - id: originate_timestamp
    type: ntp_timestamp
    doc: Time at the client when the request departed for the server.
  - id: receive_timestamp
    type: ntp_timestamp
    doc: Time at the server when the request arrived.
  - id: transmit_timestamp
    type: ntp_timestamp
    doc: Time at the server when the response left for the client.
  - id: extension_fields
    type: extension_field
    repeat: eos
    doc: Optional extension fields. Exist only if length > 48 bytes.
  - id: key_id
    type: u4
    doc: Key Identifier for authentication (optional).
  - id: message_digest
    type: u8
    doc: Message Digest for authentication (optional).

types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
        doc: Seconds since Jan 1, 1900.
      - id: fraction
        type: u4
        doc: Fraction of a second.

  extension_field:
    seq:
      - id: field_type
        type: u2
        doc: Identifier of the type of extension field.
      - id: field_length
        type: u2
        doc: Length of the extension field.
      - id: field_data
        size: field_length
        doc: Data of the extension field.

enums:
  leap_indicator:
    0: no_warning
    1: last_minute_61_seconds
    2: last_minute_59_seconds
    3: alarm_condition
  mode:
    1: symmetric_active
    2: symmetric_passive
    3: client
    4: server
    5: broadcast
    6: ntp_control_message
    7: private_use
  version_number:
    4: version_4

instances:
  leap_indicator:
    value: '((li_vn_mode >> 6) & 0x03)'
  version:
    value: '((li_vn_mode >> 3) & 0x07)'
  mode:
    value: '(li_vn_mode & 0x07)'