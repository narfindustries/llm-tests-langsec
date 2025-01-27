meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  license: CC0-1.0
  ks-version: 0.9
  endian: be
seq:
  - id: leap_indicator
    type: u2
    doc: Leap indicator (LI) - warns of an impending leap second to be inserted or deleted in the last minute of the current month.
  - id: version
    type: u3
    doc: Version number (VN) - indicates the version of the NTP protocol.
  - id: mode
    type: u3
    doc: Mode - indicates the mode of the NTP packet.
  - id: stratum
    type: u8
    doc: Stratum - indicates the stratum level of the local clock.
  - id: poll
    type: u8
    doc: Poll - indicates the maximum interval between successive messages.
  - id: precision
    type: s8
    doc: Precision - indicates the precision of the local clock.
  - id: root_delay
    type: s32
    doc: Root delay - indicates the total round-trip delay to the reference clock.
  - id: root_dispersion
    type: u32
    doc: Root dispersion - indicates the maximum error due to the clock frequency tolerance.
  - id: reference_id
    type: u32
    doc: Reference ID - identifies the reference clock.
  - id: reference_timestamp
    type: u64
    doc: Reference timestamp - indicates the time when the system clock was last set or corrected.
  - id: originate_timestamp
    type: u64
    doc: Originate timestamp - indicates the time at which the request departed the client for the server.
  - id: receive_timestamp
    type: u64
    doc: Receive timestamp - indicates the time at which the request arrived at the server.
  - id: transmit_timestamp
    type: u64
    doc: Transmit timestamp - indicates the time at which the reply departed the server for the client.
  - id: extension_fields
    type: u8[]
    size-eos: true
    doc: Extension fields - optional fields for future use.