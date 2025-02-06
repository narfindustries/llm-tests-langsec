types:
  mqtt_fixed_header:
    seq:
      - id: type
        type: u1
      - id: flags
        type: u4
      - id: remaining_length
        type: remaining_length
  mqtt_variable_header:
    seq:
      - id: packet_identifier
        type: u2
        if: (self.parent.type in [3, 4, 5, 6, 8, 10, 11, 13, 14])
  mqtt_property:
    seq:
      - id: property_id
        type: u2
      - id: property_value
        type: instance:
            switch:
              on: property_id
              cases:
                1:
                  type: u4
                2:
                  type: str
                3:
                  type: str
                4:
                  type: u4
                5:
                  type: u4
                6:
                  type: u2
                7:
                  type: u4
                8:
                  type: u2
                9:
                  type: u2
                10:
                  type: str
                11:
                  type: u2
                12:
                  type: u2
                13:
                  type: u4
                14:
                  type: u4
                15:
                  type: u4
                16:
                  type: u4
                17:
                  type: str
                18:
                  type: str
                19:
                  type: u2
                20:
                  type: u4
                21:
                  type: u4
                22:
                  type: u4
                23:
                  type: u4
                24:
                  type: u4
                25:
                  type: str
                26:
                  type: u4
                27:
                  type: str
                28:
                  type: str
                29:
                  type: str
                31:
                  type: str
                32:
                  type: u4
                33:
                  type: u4
                34:
                  type: str
                35:
                  type: str
                36:
                  type: str
                37:
                  type: str
                38:
                  type: str
                39:
                  type: str
                40:
                  type: str
                41:
                  type: str
                42:
                  type: str
                43:
                  type: u4
                44:
                  type: u4
                45:
                  type: u4
                46:
                  type: u4
                47:
                  type: u4
                48:
                  type: u4


  mqtt_connect:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: protocol_name
        type: str
      - id: protocol_level
        type: u4
      - id: connect_flags
        type: u4
      - id: keep_alive
        type: u2
      - id: client_id
        type: str
      - id: will_properties
        type: mqtt_property
        repeat: true
        if: (self.parent.connect_flags & 0b10000000)
      - id: will_topic
        type: str
        if: (self.parent.connect_flags & 0b10000000)
      - id: will_message
        type: bytes
        if: (self.parent.connect_flags & 0b10000000)
      - id: username
        type: str
        if: (self.parent.connect_flags & 0b00000100)
      - id: password
        type: bytes
        if: (self.parent.connect_flags & 0b00000100)
  mqtt_connack:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: session_present
        type: u1
      - id: connack_reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_publish:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: topic_name
        type: str
      - id: variable_header
        type: mqtt_variable_header
      - id: payload
        type: bytes
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_puback:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_pubrec:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_pubrel:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_pubcomp:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_subscribe:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: topic_filter
        type: str
      - id: qos
        type: u1
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_suback:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: reason_codes
        type: u2
        repeat: true
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_unsubscribe:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: topic_filter
        type: str
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_unsuback:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: reason_codes
        type: u2
        repeat: true
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_pingreq:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
  mqtt_pingresp:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
  mqtt_disconnect:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: properties
        type: mqtt_property
        repeat: true
  mqtt_auth:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: true

