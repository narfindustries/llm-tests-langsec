meta:
  id: mqtt
  title: MQTT Protocol Specification
  endian: be

seq:
  - id: fixed_header
    type: fixed_header

  - id: variable_header
    type: variable_header

  - id: payload
    type: payload
    size-eos: true

types:
  fixed_header:
    seq:
      - id: packet_type
        type: b4
      - id: flags
        type: b4
      - id: remaining_length
        type: vlq_base128_be

  variable_header:
    seq:
      - id: protocol_name
        type: str
        size: 4
        encoding: ascii
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2

  payload:
    seq:
      - id: client_id
        type: str
        size-eos: true