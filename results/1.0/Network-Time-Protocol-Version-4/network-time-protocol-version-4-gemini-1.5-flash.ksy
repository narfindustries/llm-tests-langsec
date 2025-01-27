$schema: https://kaitaistruct.org/schema/ksy.v1.ksy
id: network_time_protocol_version_4_gemini_1
endian: be

seq:
  - id: header
    struct:
      - id: stratum
        type: u1
      - id: poll
        type: u1
      - id: precision
        type: u1
      - id: root_delay
        type: f4
      - id: root_dispersion
        type: f4
      - id: reference_id
        type: u4
      - id: reference_timestamp
        type: f8
      - id: origin_timestamp
        type: f8
      - id: receive_timestamp
        type: f8
      - id: transmit_timestamp
        type: f8

