meta:
  id: ntp_v4
  endian: big
  title: Network Time Protocol Version 4
seq:
  - id: flags
    type: u1
    doc: |
      Leap Indicator (2 bits) | Version Number (3 bits) | Mode (3 bits)
  - id: stratum
    type: u1
    doc: Stratum level of the clock
  - id: poll
    type: s1
    doc: Maximum interval between successive messages (log2 seconds)
  - id: precision
    type: s1
    doc: Precision of the clock (log2 seconds)
  - id: root_delay
    type: s4
    doc: Total round trip delay time (seconds)
  - id: root_dispersion
    type: s4
    doc: Maximum error due to clock frequency tolerance (seconds)
  - id: reference_id
    type: u4
    doc: Reference clock identifier
  - id: reference_timestamp
    type: ntp_timestamp
    doc: Time when the clock was last set or corrected
  - id: origin_timestamp
    type: ntp_timestamp
    doc: Time at the client when the request departed for the server
  - id: receive_timestamp
    type: ntp_timestamp
    doc: Time at the server when the request arrived from the client
  - id: transmit_timestamp
    type: ntp_timestamp
    doc: Time at the server when the response left for the client
  - id: extension_field
    type: extension
    if: _root.flags & 0x10 != 0
    doc: Optional extension field
  - id: key_identifier
    type: u4
    if: _root.flags & 0x20 != 0
    doc: Optional key identifier
  - id: message_digest
    size: 16
    if: _root.flags & 0x20 != 0
    doc: Optional message digest (MD5)

types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u4
        doc: Seconds since January 1, 1900
      - id: fraction
        type: u4
        doc: Fraction of second

  extension:
    seq:
      - id: field_type
        type: u2
        doc: Field type
      - id: length
        type: u2
        doc: Length of the value field
      - id: value
        size: length
        doc: Value