meta:
  id: network_time_protocol_version_4
  title: Network Time Protocol Version 4
  license: MIT
  ks-version: 0.9
  endian: be

seq:
  - id: leap_indicator
    type: u2
    doc: Leap indicator (LI) - warns of an impending leap second
  - id: version
    type: u3
    doc: Version number (VN) - version of the NTP protocol
  - id: mode
    type: u3
    doc: Mode - mode of operation
  - id: stratum
    type: u8
    doc: Stratum - level of the clock in the hierarchy
  - id: poll
    type: u8
    doc: Poll interval - maximum interval between successive messages
  - id: precision
    type: s8
    doc: Precision - precision of the local clock
  - id: root_delay
    type: s32
    doc: Root delay - total round-trip delay to the reference clock
  - id: root_dispersion
    type: u32
    doc: Root dispersion - maximum error relative to the reference clock
  - id: reference_id
    type: u32
    doc: Reference ID - identifier of the reference clock
  - id: reference_timestamp
    type: ntp_timestamp
    doc: Reference timestamp - time when the system clock was last set or corrected
  - id: originate_timestamp
    type: ntp_timestamp
    doc: Originate timestamp - time at the client when the request departed for the server
  - id: receive_timestamp
    type: ntp_timestamp
    doc: Receive timestamp - time at the server when the request arrived from the client
  - id: transmit_timestamp
    type: ntp_timestamp
    doc: Transmit timestamp - time at the server when the response left for the client

types:
  ntp_timestamp:
    seq:
      - id: seconds
        type: u32
        doc: Seconds since Jan 1, 1900
      - id: fraction
        type: u32
        doc: Fraction of a second