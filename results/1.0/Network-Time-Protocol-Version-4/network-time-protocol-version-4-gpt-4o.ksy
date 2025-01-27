meta:
  id: network_time_protocol_v4
  title: Network Time Protocol Version 4
  application: ntp
  file-extension: nts
  endian: big
seq:
  - id: flags
    type: b1
    doc: |
      The flags field of the NTP packet.
      It combines the leap indicator, version number, and mode.
  - id: stratum
    type: u1
    doc: |
      This field indicates the stratum level of the local clock.
  - id: poll
    type: s1
    doc: |
      This field represents the maximum interval between successive messages, in log2 seconds.
  - id: precision
    type: s1
    doc: |
      This field represents the precision of the local clock, in log2 seconds.
  - id: root_delay
    type: u4
    doc: |
      Total round-trip delay to the primary reference source, in NTP short format.
  - id: root_dispersion
    type: u4
    doc: |
      Total dispersion to the primary reference source, in NTP short format.
  - id: reference_id
    type: u4
    doc: |
      A 32-bit identifier for the particular server or reference clock.
  - id: reference_timestamp
    type: u8
    doc: |
      The time the system clock was last set or corrected, in NTP timestamp format.
  - id: originate_timestamp
    type: u8
    doc: |
      The time at which the request departed the client for the server, in NTP timestamp format.
  - id: receive_timestamp
    type: u8
    doc: |
      The time at which the request arrived at the server, in NTP timestamp format.
  - id: transmit_timestamp
    type: u8
    doc: |
      The time at which the reply departed the server for the client, in NTP timestamp format.
  - id: key_id
    type: u4
    doc: |
      Key identifier used by the client or server to select the key used to generate the message authentication code.
  - id: message_digest
    type: bytes
    size: 16
    doc: |
      The message authentication code used to verify the authenticity of the message.