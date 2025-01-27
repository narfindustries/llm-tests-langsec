meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be
seq:
  - id: leap_indicator
    type: b2
    doc: Warning of an impending leap second
  - id: version
    type: b3
    doc: NTP version number
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
    doc: Clock precision in log2 seconds
  - id: root_delay
    type: u4
    doc: Total round trip delay to the reference clock
  - id: root_dispersion
    type: u4
    doc: Maximum error relative to the reference clock
  - id: reference_id
    type: u4
    doc: Reference identifier
  - id: reference_time
    type: u8
    doc: Time when the system clock was last set or corrected
  - id: originate_time
    type: u8
    doc: Time at the client when the request was sent
  - id: receive_time
    type: u8
    doc: Time at the server when the request was received
  - id: transmit_time
    type: u8
    doc: Time at the server when the response was sent
  - id: extension_fields
    type: extension_field
    repeat: eos
    doc: Optional extension fields
  - id: authentication_data
    type: auth_data
    doc: Optional authentication data

types:
  extension_field:
    seq:
      - id: field_type
        type: u2
      - id: field_length
        type: u2
      - id: field_value
        size: field_length
      - id: padding
        size: '(4 - (field_length % 4)) % 4'

  auth_data:
    seq:
      - id: key_id
        type: u4
        doc: Cryptographic key identifier
      - id: message_digest
        size: 16
        doc: MD5 message digest