types:
  mqtt_fixed_header:
    seq:
      - id: message_type
        type: u1
      - id: flags
        type: u1
      - id: remaining_length
        type: remaining_length
  mqtt_variable_header:
    seq:
      - id: packet_identifier
        type: u2
        if: message_type in [3, 4, 5, 6, 8, 9, 10, 11, 12]
  mqtt_property:
    seq:
      - id: property_id
        type: u2
      - id: property_value
        type: mqtt_property_value
        if: property_id != 0
  mqtt_property_value:
    switch_on: property_id
    cases:
      1:
        type: u1
      2:
        type: u2
      3:
        type: u4
      4:
        type: u8
      5:
        type: str
        encoding: UTF-8
      6:
        type: bytes
      7:
        type: u2
      8:
        type: u4
      # ... Add other property types with appropriate encodings and sizes as needed ...

  mqtt_connect:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: protocol_name
        type: str
        encoding: UTF-8
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive_timer
        type: u2
      - id: client_identifier
        type: str
        encoding: UTF-8
      - id: will_properties
        type: mqtt_property_list
        if: connect_flags & 0x04
      - id: will_topic
        type: str
        encoding: UTF-8
        if: connect_flags & 0x04
      - id: will_message
        type: bytes
        if: connect_flags & 0x04
      - id: username
        type: str
        encoding: UTF-8
        if: connect_flags & 0x80
      - id: password
        type: bytes
        if: connect_flags & 0x40
  mqtt_connack:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: session_present
        type: u1
      - id: connect_return_code
        type: u1
      - id: properties
        type: mqtt_property_list
  mqtt_publish:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: topic_name
        type: str
        encoding: UTF-8
      - id: properties
        type: mqtt_property_list
      - id: payload
        type: bytes
        size: remaining_length - (topic_name.size + properties.size) # Adjust size calculation as needed

  mqtt_puback:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: properties
        type: mqtt_property_list
  mqtt_pubrec:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: properties
        type: mqtt_property_list
  mqtt_pubrel:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: properties
        type: mqtt_property_list
  mqtt_pubcomp:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: properties
        type: mqtt_property_list
  mqtt_subscribe:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: topic_filters
        type: mqtt_topic_filter_list
      - id: properties
        type: mqtt_property_list
  mqtt_suback:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: return_codes
        type: mqtt_return_code_list
      - id: properties
        type: mqtt_property_list
  mqtt_unsubscribe:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: topic_filters
        type: mqtt_topic_filter_list
      - id: properties
        type: mqtt_property_list
  mqtt_unsuback:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: variable_header
        type: mqtt_variable_header
      - id: properties
        type: mqtt_property_list
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
        type: mqtt_property_list
  mqtt_topic_filter:
    seq:
      - id: topic_filter
        type: str
        encoding: UTF-8
      - id: qos
        type: u1
  mqtt_topic_filter_list:
    seq:
      - id: topic_filters
        type: mqtt_topic_filter
        repeat: true
  mqtt_return_code_list:
    seq:
      - id: return_codes
        type: u1
        repeat: true
  mqtt_property_list:
    seq:
      - id: properties
        type: mqtt_property
        repeat: true
  remaining_length:
    process: mqtt_remaining_length
    endian: be
    expr: v1 + (v2 << 7) + (v3 << 14) + (v4 << 21)
    instances:
      - v1: u7
      - v2: u7
      - v3: u7
      - v4: u7


The error "error: 'size', 'size-eos' or 'terminator' must be specified" arises because the `payload` field in the `mqtt_publish` type needs a way to determine its length.  The `remaining_length` field in the fixed header gives the total length of the variable part of the message, but we need to subtract the lengths of other fields (`topic_name` and `properties`) to find the payload size.  The provided `size` expression attempts to do this.  However, this calculation relies on the `size` property of the nested types, which might not be explicitly defined.  You'll likely need to add `size` or `size-eos` attributes to other fields as needed to make the size calculation work correctly.  The exact calculation might need adjustment depending on the encoding and structure of the properties.  This is a complex structure, and careful review of the MQTT 5.0 specification is crucial for accurate size determination.
