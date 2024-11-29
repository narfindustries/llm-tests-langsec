meta:
  id: ntp_v4
  title: Network Time Protocol (NTP) Version 4
  application: Time synchronization
  file-extension: ntp
  endian: big
  description: |
    This is a Kaitai Struct specification for parsing NTPv4 headers. The structure is based on RFC 5905.

seq:
  - id: leap_indicator
    type: b2
    description: Leap indicator (LI), 2 bits.

  - id: version_number
    type: b3
    description: NTP version number, 3 bits.

  - id: mode
    type: b3
    description: Mode (client, server, broadcast, etc.), 3 bits.

  - id: stratum
    type: u1
    description: Stratum level of the local clock.

  - id: poll_interval
    type: s1
    description: Poll interval in log2 seconds.

  - id: precision
    type: s1
    description: Precision of the system clock in log2 seconds.

  - id: root_delay
    type: u4
    description: Total round-trip delay to the primary reference source, in fixed-point format.

  - id: root_dispersion
    type: u4
    description: Total dispersion to the primary reference source, in fixed-point format.

  - id: reference_id
    type: u4
    description: Identifier of the reference clock or server.

  - id: reference_timestamp
    type: u8
    description: Timestamp of the last synchronization event, in NTP's 64-bit timestamp format.

  - id: originate_timestamp
    type: u8
    description: Originate timestamp (T1), the time the request left the client.

  - id: receive_timestamp
    type: u8
    description: Receive timestamp (T2), the time the request was received by the server.

  - id: transmit_timestamp
    type: u8
    description: Transmit timestamp (T3), the time the response left the server.

  - id: key_identifier
    type: u4
    if: _io.size > 48
    description: Optional field for NTP authentication.

  - id: message_digest
    size: 16
    if: _io.size > 52
    description: Optional field for NTP authentication.

instances:
  root_delay_seconds:
    value: (root_delay >> 16) + ((root_delay & 0xffff) / 65536.0)
    description: Root delay in seconds as a floating-point value.

  root_dispersion_seconds:
    value: (root_dispersion >> 16) + ((root_dispersion & 
