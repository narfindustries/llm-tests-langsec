meta:
  id: mqtt_packet
  title: MQTT Packet
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header

  - id: remaining_length
    type: vlq_int
    doc: "Length of variable header + payload"

  - id: variable_header_and_payload
    size: remaining_length.value
    type: variable_header_and_payload

types:
  fixed_header:
    seq:
      - id: control_packet_type
        type: u1
        mask: 0xF0
        enum: control_packet_type

      - id: flags
        type: u1
        mask: 0x0F
      - id: remaining_length_prefix
        type: vlq_int
        doc: "Remaining length in MQTT fixed header"

  control_packet_type:
    1: connect
    2: connack
    3: publish
    4: puback
    5: pubrec
    6: pubrel
    7: pubcomp
    8: subscribe
    9: suback
    10: unsubscribe
    11: unsuback
    12: pingreq
    13: pingresp
    14: disconnect

  vlq_int:
    seq:
      - id: byte
        type: u1
        doc: |
          Continuation bit is highest bit of byte. Loop until this bit is 0.
    repeat: until 
    repeat-until: byte & 0x80 == 0

    instances:
      value:
        value: |
          (0..(repeat_expr.length)).map { |i|
            (repeat_expr[i].byte & 0x7F) << (7 * i)
          }.sum

  variable_header_and_payload:
    seq:
      - id: variable_header
        type: variable_header
        size: 'header_size'

      - id: payload
        size-eos: true
        type: payload

    instances:
      header_size:
        value: "_root.remaining_length.value - _root.length_of_variable_header"

  variable_header:
    seq:
      - id: protocol_name
        type: str
        size: 6
      - id: protocol_level
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive
        type: u2

  payload:
    doc: "Generic payload placeholder. Extend with specific handling if needed."
    size-eos: true
    type: bytes