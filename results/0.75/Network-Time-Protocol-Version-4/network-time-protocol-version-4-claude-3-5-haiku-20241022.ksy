meta:
  id: ntp_v4
  title: Network Time Protocol Version 4
  endian: be
seq:
  - id: leap_indicator
    type: b2
    doc: Leap second indicator
  - id: version
    type: b3
    doc: NTP version number
  - id: mode
    type: b3
    doc: Association mode
  - id: stratum
    type: u1
    doc: Stratum level of clock
  - id: poll
    type: s1
    doc: Poll interval in log2 seconds
  - id: precision
    type: s1
    doc: Clock precision in log2 seconds
  - id: root_delay
    type: u4
    doc: Total round trip delay to primary source
  - id: root_dispersion
    type: u4
    doc: Maximum error relative to primary source
  - id: reference_id
    type: u4
    doc: Reference identifier
  - id: reference_timestamp
    type: u8
    doc: Time when system clock was last updated
  - id: originate_timestamp
    type: u8
    doc: Time when request was sent from client
  - id: receive_timestamp
    type: u8
    doc: Time when request was received by server
  - id: transmit_timestamp
    type: u8
    doc: Time when response was sent from server
  - id: authenticator
    type: authenticator
    if: mode != 0
    doc: Optional authentication data
types:
  authenticator:
    seq:
      - id: key_id
        type: u4
        doc: Cryptographic key identifier
      - id: message_digest
        size: 16
        doc: Message authentication code