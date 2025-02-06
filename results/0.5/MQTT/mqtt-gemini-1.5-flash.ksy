types:
  mqtt_fixed_header:
    seq:
      - id: type
        type: u1
      - id: flags
        type: u1
      - id: remaining_length
        type: remaining_length
  mqtt_property:
    seq:
      - id: property_id
        type: u2
      - id: property_length
        type: u4le
      - id: property_value
        type: bytes
        size: property_length
  mqtt_connect:
    seq:
      - id: fixed_header
        type: mqtt_fixed_header
      - id: protocol_name
        type: str
        encoding: UTF-8
        size: 4
      - id: protocol_version
        type: u1
      - id: connect_flags
        type: u1
      - id: keep_alive_timer
        type: u2be
      - id: client_identifier
        type: str
        encoding: UTF-8
      - id: will_properties
        type: mqtt_property_list
        if: connect_flags & 0x04 != 0 # Will flag check
      - id: authentication_method
        type: str
        encoding: UTF-8
        if: connect_flags & 0x80 != 0 # Authentication check
      - id: authentication_data
        type: bytes
        if: connect_flags & 0x80 != 0 # Authentication check
      - id: session_expiry_interval
        type: u4be
        if: connect_flags & 0x02 != 0 # Session expiry check
      - id: receive_maximum
        type: u2be
        if: connect_flags & 0x01 != 0 # Receive maximum check
      - id: topic_alias_maximum
        type: u2be
        if: connect_flags & 0x01 != 0 # Topic alias maximum check
      - id: maximum_packet_size
        type: u4be
        if: connect_flags & 0x01 != 0 # Maximum packet size check
      - id: will_delay_interval
        type: u4be
        if: connect_flags & 0x04 != 0 # Will delay check
      - id: request_response_information
        type: u1
        if: connect_flags & 0x01 != 0 # Request response check
      - id: request_problem_information
        type: u1
        if: connect_flags & 0x01 != 0 # Request problem check

  mqtt_property_list:
    seq:
      - id: properties
        type: mqtt_property
        repeat: eos
  # ... (Add other MQTT packet types: CONNACK, PUBLISH, PUBACK, etc.  similarly) ...

  remaining_length:
    process: mqtt_remaining_length
    endian: be

  mqtt_remaining_length:
    expr: self.readU8()
    process: mqtt_remaining_length_process

  mqtt_remaining_length_process:
    expr: (self._io.readU8() & 0x7F) + (self._io.readU8() & 0x7F) * 128 + (self._io.readU8() & 0x7F) * 128 * 128 + (self._io.readU8() & 0x7F) * 128 * 128 * 128

