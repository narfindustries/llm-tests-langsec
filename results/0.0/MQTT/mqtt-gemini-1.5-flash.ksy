types:
  mqtt_fixed_header:
    seq:
      - id: message_type
        type: u1
      - id: flags
        type: u1
      - id: remaining_length
        type: mqtt_remaining_length
  mqtt_remaining_length:
    seq:
      - id: byte
        type: u1
        repeat: eos
    process: decode_remaining_length
  mqtt_property:
    seq:
      - id: property_id
        type: u2
      - id: property_length
        type: u4
      - id: property_value
        type: mqtt_property_value
        size: property_length
  mqtt_property_value:
    switch-on: property_id
    cases:
      # This section needs to be completed with all MQTT property types and their corresponding Kaitai Struct types.
      1:
        type: str
      2:
        type: u4
      3:
        type: u2
      # ... Add all other property types here ...

  mqtt_connect_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: protocol_name
        type: str
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive_timer
        type: u2
      - id: client_identifier
        type: str
      - id: will_properties
        type: mqtt_property
        repeat: eos
      - id: authentication_method
        type: str
      - id: authentication_data
        type: bytes
      - id: session_expiry_interval
        type: u4
      - id: receive_maximum
        type: u2
      - id: topic_alias_maximum
        type: u2
      - id: maximum_packet_size
        type: u4
      - id: will_delay_interval
        type: u4
      - id: request_response_information
        type: u1
      - id: request_problem_information
        type: u1
      - id: user_properties
        type: mqtt_property
        repeat: eos
  mqtt_connack_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: session_present
        type: u1
      - id: connect_return_code
        type: u1
      - id: session_expiry_interval
        type: u4
      - id: receive_maximum
        type: u2
      - id: topic_alias_maximum
        type: u2
      - id: maximum_packet_size
        type: u4
      - id: assigned_client_identifier
        type: str
      - id: server_keep_alive
        type: u2
      - id: authentication_method
        type: str
      - id: user_properties
        type: mqtt_property
        repeat: eos
  mqtt_publish_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: topic_name
        type: str
      - id: packet_identifier
        type: u2
      - id: qos_level
        type: u1
      - id: retain_flag
        type: u1
      - id: payload
        type: bytes
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_puback_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_pubrec_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_pubrel_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_pubcomp_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_subscribe_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: topic_filter
        type: str
        repeat: eos
      - id: qos
        type: u1
        repeat: eos
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_suback_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: return_codes
        type: u1
        repeat: eos
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_unsubscribe_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: topic_filter
        type: str
        repeat: eos
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_unsuback_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: packet_identifier
        type: u2
      - id: reason_codes
        type: u1
        repeat: eos
      - id: properties
        type: mqtt_property
        repeat: eos
  mqtt_pingreq_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
  mqtt_pingresp_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
  mqtt_disconnect_message:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: reason_code
        type: u2
      - id: properties
        type: mqtt_property
        repeat: eos

functions:
  decode_remaining_length:
    params:
      - bytes: byte
    code: |
      var multiplier = 1;
      var value = 0;
      for (var i = 0; i < this.bytes.length; i++) {
        var byte = this.bytes[i];
        value += (byte & 0x7F) * multiplier;
        multiplier *= 128;
        if ((byte & 0x80) == 0) {
          break;
        }
      }
      return value;
