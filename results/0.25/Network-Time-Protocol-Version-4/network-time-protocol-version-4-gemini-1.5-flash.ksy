types:
  - id: ntp_timestamp
    struct:
      - id: seconds
        type: u8
      - id: fraction
        type: u8
  - id: ntp_v4
    struct:
      - id: li
        type: bits
        size: 2
      - id: vn
        type: bits
        size: 3
      - id: mode
        type: bits
        size: 3
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
        type: ntp_timestamp
      - id: originate_timestamp
        type: ntp_timestamp
      - id: receive_timestamp
        type: ntp_timestamp
      - id: transmit_timestamp
        type: ntp_timestamp
      - id: extension
        type: seq
        repeat: expr
        repeat-expr: extension_len > 0
        size: extension_len
        struct:
          - id: extension_type
            type: u2
          - id: extension_len
            type: u2
          - id: extension_data
            type: bytes
            size: extension_len

