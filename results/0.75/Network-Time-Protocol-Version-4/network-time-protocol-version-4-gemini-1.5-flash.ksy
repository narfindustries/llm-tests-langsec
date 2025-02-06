type: struct
id: ntpv4
fields:
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
    type: s1
  - id: root_delay
    type: f4
  - id: root_dispersion
    type: f4
  - id: reference_id
    type: u4
  - id: reference_timestamp
    type: f8
  - id: originate_timestamp
    type: f8
  - id: receive_timestamp
    type: f8
  - id: transmit_timestamp
    type: f8
  - id: extension
    type: seq
    size: lambda (this.extension_len)
    repeat: expr (this.extension_len > 0)
    fields:
      - id: extension_type
        type: u2
      - id: extension_len
        type: u2
      - id: extension_data
        type: bytes
        size: lambda (this.extension_len)

