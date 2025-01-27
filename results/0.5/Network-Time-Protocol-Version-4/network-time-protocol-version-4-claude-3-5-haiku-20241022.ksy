meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  file-extension: ntp
  endian: be

seq:
  - id: leap_indicator
    type: b2
    doc: Leap second indicator
  
  - id: version
    type: b3
    doc: NTP protocol version
  
  - id: mode
    type: b3
    doc: NTP mode of operation
  
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
    type: f4
    doc: Total round-trip delay to primary source

  - id: root_dispersion
    type: f4
    doc: Maximum error relative to primary source

  - id: reference_identifier
    type: str
    size: 4
    encoding: ASCII
    doc: Reference clock identifier

  - id: reference_timestamp
    type: u8
    doc: Time when system was last synchronized

  - id: originate_timestamp
    type: u8
    doc: Time request was sent from client

  - id: receive_timestamp
    type: u8
    doc: Time request was received by server

  - id: transmit_timestamp
    type: u8
    doc: Time reply was sent from server

instances:
  ntp_timestamp:
    value: transmit_timestamp
    doc: Convenience accessor for transmit timestamp