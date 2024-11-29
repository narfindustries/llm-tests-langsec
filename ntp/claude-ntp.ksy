meta:
  id: ntp_packet
  title: Network Time Protocol (NTP) Packet
  license: MIT
  endian: be

seq:
  - id: leap_indicator
    type: b2
    doc: |
      Leap second indicator:
      - 0: no warning
      - 1: last minute of the day has 61 seconds
      - 2: last minute of the day has 59 seconds
      - 3: alarm condition (clock not synchronized)

  - id: version
    type: b3
    doc: NTP version number (currently 4)

  - id: mode
    type: b3
    doc: |
      Mode of operation:
      - 0: reserved
      - 1: symmetric active
      - 2: symmetric passive
      - 3: client
      - 4: server
      - 5: broadcast
      - 6: reserved for NTP control messages
      - 7: reserved for private use

  - id: stratum
    type: u1
    doc: |
      Stratum level of the clock:
      - 0: unspecified
      - 1: primary reference (e.g., atomic clock)
      - 2-255: secondary reference (synchronized to another source)

  - id: poll_interval
    type: s1
    doc: Maximum interval between successive messages (log2 seconds)

  - id: precision
    type: s1
    doc: Clock precision (log2 seconds)

  - id: root_delay
    type: f4
    doc: Total round-trip delay to the reference clock

  - id: root_dispersion
    type: f4
    doc: Maximum error relative to the reference clock

  - id: reference_identifier
    type: u4
    doc: Reference source identifier (depends on stratum)

  - id: reference_timestamp
    type: u8
    doc: Time when the system was last set or corrected

  - id: originate_timestamp
    type: u8
    doc: Time when the request was sent from the client

  - id: receive_timestamp
    type: u8
    doc: Time when the request was received by the server

  - id: transmit_timestamp
    type: u8
    doc: Time when the reply was sent from the server

  # Optional authentication data (not part of standard NTP)
  - id: authentication_data
    type: authentication_block
    repeat: expr
    repeat-expr: 0  # Placeholder, actual authentication not typically used in basic NTP
    doc: Optional authentication data (rarely used in modern implementations)

types:
  authentication_block:
    seq:
      - id: key_identifier
        type: u4
        doc: Authentication key identifier
      - id: message_digest
        type: u4
        size: 16
        doc: MD5 message digest