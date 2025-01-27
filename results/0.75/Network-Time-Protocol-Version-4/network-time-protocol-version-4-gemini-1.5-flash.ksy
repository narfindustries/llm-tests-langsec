$schema: https://kaitaistruct.org/schema/ksy.yaml#
id: network_time_protocol_version_4_gemini_1
endian: be
seq:
  - id: header
    type: seq
    seq:
      - id: flags
        type: u1
      - id: leap
        type: u1
      - id: mode
        type: u1
      - id: stratum
        type: u1
      - id: poll
        type: u1
      - id: precision
        type: u1
      - id: root_delay
        type: u4
      - id: root_dispersion
        type: u4
      - id: reference_id
        type: u4
      - id: reference_timestamp
        type: u8
      - id: origin_timestamp
        type: u8
      - id: receive_timestamp
        type: u8
      - id: transmit_timestamp
        type: u8

