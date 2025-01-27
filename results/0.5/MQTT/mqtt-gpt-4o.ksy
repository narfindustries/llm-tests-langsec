meta:
  id: mqtt_packet
  title: MQTT Packet
  file-extension: mqtt
  endian: be

enums:
  packet_type:
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
    15: auth

seq:
  - id: fixed_header
    type: fixed_header

types:
  fixed_header:
    seq:
      - id: byte1
        type: u1
      - id: remaining_length
        type: vlq_base128_be
    instances:
      packet_type:
        value: 'byte1 >> 4'
        enum: packet_type

  vlq_base128_be:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: '((value & 0x80) == 0)'
    instances:
      len_value:
        value: 'value.size'
      value_decoded:
        value: 'value.fold(0) {|acc, i| (acc << 7) | (i & 0x7f)}'