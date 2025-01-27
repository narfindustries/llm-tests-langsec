meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be

seq:
  - id: leap_indicator
    type: b2
    doc: Warning of an impending leap second to be inserted/deleted
  - id: version
    type: b3
    doc: NTP version number (4)
  - id: mode
    type: b3
    doc: Association mode
  - id: stratum
    type: u1
    doc: Stratum level of the local clock
  - id: poll
    type: s1
    doc: Poll interval in log2 seconds
  - id: precision
    type: s1
    doc: Precision of the local clock in log2 seconds
  - id: root_delay
    type: u4
    doc: Total round-trip delay to the reference clock
  - id: root_dispersion
    type: u4
    doc: Maximum error relative to the reference clock
  - id: reference_identifier
    type: u4
    doc: Reference source identifier
  - id: reference_timestamp
    type: u8
    doc: Time when the system clock was last set or corrected
  - id: originate_timestamp
    type: u8
    doc: Time when the request was sent from the client
  - id: receive_timestamp
    type: u8
    doc: Time when the request was received by the server
  - id: transmit_timestamp
    type: u8
    doc: Time when the response was sent from the server
  - id: extensions
    type: extension
    repeat: eos
    doc: Optional extension fields
  - id: authentication
    type: auth_data
    doc: Optional authentication data

types:
  extension:
    seq:
      - id: field_type
        type: u2
      - id: field_length
        type: u2
      - id: data
        size: field_length
        doc: Extension field data

  auth_data:
    seq:
      - id: key_identifier
        type: u4
        doc: Authentication key identifier
      - id: message_digest
        size: 16
        doc: MD5 message digest