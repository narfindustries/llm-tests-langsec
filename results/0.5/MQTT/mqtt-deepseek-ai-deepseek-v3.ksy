meta:
  id: mqtt
  title: MQTT Protocol
  license: CC0-1.0
  endian: be
seq:
  - id: fixed_header
    type: fixed_header
  - id: variable_header
    type: variable_header
    if: fixed_header.packet_type != 0 and fixed_header.packet_type != 12 and fixed_header.packet_type != 13
  - id: payload
    type: payload
    if: fixed_header.packet_type == 3 or fixed_header.packet_type == 8 or fixed_header.packet_type == 10 or fixed_header.packet_type == 14
types:
  fixed_header:
    seq:
      - id: packet_type
        type: u1
        enum: packet_type
      - id: flags
        type: u1
        if: packet_type == 3 or packet_type == 8 or packet_type == 10 or packet_type == 14
      - id: remaining_length
        type: vbi
  variable_header:
    seq:
      - id: properties_length
        type: vbi
      - id: properties
        type: properties
        size: properties_length
    instances:
      packet_identifier:
        value: 'packet_type == 4 or packet_type == 5 or packet_type == 6 or packet_type == 7 or packet_type == 8 or packet_type == 9 or packet_type == 10 or packet_type == 11 ? _io.read_u2() : null'
  payload:
    seq:
      - id: data
        size-eos: true
  properties:
    seq:
      - id: payload_format_indicator
        type: u1
        if: _parent._parent.fixed_header.packet_type == 3
      - id: message_expiry_interval
        type: u4
        if: _parent._parent.fixed_header.packet_type == 3
      - id: content_type
        type: str_utf8
        if: _parent._parent.fixed_header.packet_type == 3
      - id: response_topic
        type: str_utf8
        if: _parent._parent.fixed_header.packet_type == 3
      - id: correlation_data
        type: bytes
        size: _parent._parent.fixed_header.remaining_length - _parent._parent.variable_header.properties_length
        if: _parent._parent.fixed_header.packet_type == 3
      - id: subscription_identifier
        type: vbi
        if: _parent._parent.fixed_header.packet_type == 3
      - id: session_expiry_interval
        type: u4
        if: _parent._parent.fixed_header.packet_type == 1
      - id: assigned_client_identifier
        type: str_utf8
        if: _parent._parent.fixed_header.packet_type == 2
      - id: server_keep_alive
        type: u2
        if: _parent._parent.fixed_header.packet_type == 2
      - id: authentication_method
        type: str_utf8
        if: _parent._parent.fixed_header.packet_type == 1 or _parent._parent.fixed_header.packet_type == 14
      - id: authentication_data
        type: bytes
        size: _parent._parent.fixed_header.remaining_length - _parent._parent.variable_header.properties_length
        if: _parent._parent.fixed_header.packet_type == 1 or _parent._parent.fixed_header.packet_type == 14
      - id: request_problem_information
        type: u1
        if: _parent._parent.fixed_header.packet_type == 1
      - id: will_delay_interval
        type: u4
        if: _parent._parent.fixed_header.packet_type == 1
      - id: request_response_information
        type: u1
        if: _parent._parent.fixed_header.packet_type == 1
      - id: response_information
        type: str_utf8
        if: _parent._parent.fixed_header.packet_type == 2
      - id: server_reference
        type: str_utf8
        if: _parent._parent.fixed_header.packet_type == 2
      - id: reason_string
        type: str_utf8
        if: _parent._parent.fixed_header.packet_type == 2 or _parent._parent.fixed_header.packet_type == 4 or _parent._parent.fixed_header.packet_type == 5 or _parent._parent.fixed_header.packet_type == 6 or _parent._parent.fixed_header.packet_type == 7 or _parent._parent.fixed_header.packet_type == 9 or _parent._parent.fixed_header.packet_type == 11 or _parent._parent.fixed_header.packet_type == 14
      - id: receive_maximum
        type: u2
        if: _parent._parent.fixed_header.packet_type == 1
      - id: topic_alias_maximum
        type: u2
        if: _parent._parent.fixed_header.packet_type == 1
      - id: topic_alias
        type: u2
        if: _parent._parent.fixed_header.packet_type == 3
      - id: maximum_qos
        type: u1
        if: _parent._parent.fixed_header.packet_type == 2
      - id: retain_available
        type: u1
        if: _parent._parent.fixed_header.packet_type == 2
      - id: user_property
        type: str_utf8
        repeat: expr
        repeat-expr: _parent._parent.fixed_header.remaining_length - _parent._parent.variable_header.properties_length
        if: _parent._parent.fixed_header.packet_type == 1 or _parent._parent.fixed_header.packet_type == 2 or _parent._parent.fixed_header.packet_type == 3 or _parent._parent.fixed_header.packet_type == 4 or _parent._parent.fixed_header.packet_type == 5 or _parent._parent.fixed_header.packet_type == 6 or _parent._parent.fixed_header.packet_type == 7 or _parent._parent.fixed_header.packet_type == 8 or _parent._parent.fixed_header.packet_type == 9 or _parent._parent.fixed_header.packet_type == 10 or _parent._parent.fixed_header.packet_type == 11 or _parent._parent.fixed_header.packet_type == 14
      - id: maximum_packet_size
        type: u4
        if: _parent._parent.fixed_header.packet_type == 1
      - id: wildcard_subscription_available
        type: u1
        if: _parent._parent.fixed_header.packet_type == 2
      - id: subscription_identifier_available
        type: u1
        if: _parent._parent.fixed_header.packet_type == 2
      - id: shared_subscription_available
        type: u1
        if: _parent._parent.fixed_header.packet_type == 2
  vbi:
    seq:
      - id: byte
        type: u1
        repeat: until
        repeat-until: '(byte & 0x80) == 0'
    instances:
      value:
        value: 'byte & 0x7f | (byte & 0x80 ? (byte & 0x7f) << 7 : 0)'
  packet_type:
    enum:
      0: reserved
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