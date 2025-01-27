meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  license: CC0-1.0
  ks-version: 0.9
  endian: be

seq:
  - id: leap_indicator
    type: u2
    doc: Leap indicator (LI) - warns of an impending leap second.
  - id: version
    type: u3
    doc: Version number (VN) - indicates the version of the NTP protocol.
  - id: mode
    type: u3
    doc: Mode - indicates the mode of the NTP packet.
  - id: stratum
    type: u8
    doc: Stratum - indicates the level of the server in the NTP hierarchy.
  - id: poll
    type: u8
    doc: Poll interval - indicates the maximum interval between successive messages.
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
    doc: Originate timestamp - indicates the time at which the request departed the client.
  - id: receive_timestamp
    type: u64
    doc: Receive timestamp - indicates the time at which the request arrived at the server.
  - id: transmit_timestamp
    type: u64
    doc: Transmit timestamp - indicates the time at which the reply departed the server.
  - id: extension_fields
    type: u8[]
    size-eos: true
    doc: Extension fields - optional fields for future use.

types:
  u2:
    seq:
      - id: value
        type: b2
  u3:
    seq:
      - id: value
        type: b3
  u8:
    seq:
      - id: value
        type: b8
  s8:
    seq:
      - id: value
        type: b8
  s32:
    seq:
      - id: value
        type: b32
  u32:
    seq:
      - id: value
        type: b32
  u64:
    seq:
      - id: value
        type: b64
  b2:
    seq:
      - id: bits
        type: b2
  b3:
    seq:
      - id: bits
        type: b3
  b8:
    seq:
      - id: bits
        type: b8
  b32:
    seq:
      - id: bits
        type: b32
  b64:
    seq:
      - id: bits
        type: b64