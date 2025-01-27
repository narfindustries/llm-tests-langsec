meta:
  id: mqtt
  title: MQTT Protocol
  file-extension: mqtt
  endian: be

seq:
  - id: fixed_header
    type: fixed_header

  - id: variable_header
    type: variable_header
    if: fixed_header.packet_type == 1 # CONNECT packet

  - id: payload
    type: payload
    if: fixed_header.packet_type == 1 # CONNECT packet

types:
  fixed_header:
    seq:
      - id: packet_type
        type: b1
        doc: |
          The packet type and flags. The high nibble is the packet type, and the low nibble is the flags.

      - id: remaining_length
        type: vlq_base128
        doc: |
          The remaining length of the packet, which is the length of the variable header and payload.

  variable_header:
    seq:
      - id: protocol_name
        type: str
        encoding: utf-8
        size: 4
        doc: |
          The protocol name, which is always "MQTT".

      - id: protocol_level
        type: u1
        doc: |
          The protocol level, which is 4 for MQTT 3.1.1.

      - id: connect_flags
        type: b1
        doc: |
          The connect flags, which include flags for clean session, will, will QoS, will retain, password, and username.

      - id: keep_alive
        type: u2
        doc: |
          The keep alive time in seconds.

  payload:
    seq:
      - id: client_id
        type: str
        encoding: utf-8
        size: client_id_length
        doc: |
          The client identifier, which is a UTF-8 encoded string.

      - id: will_topic
        type: str
        encoding: utf-8
        size: will_topic_length
        if: variable_header.connect_flags & 0x04 != 0
        doc: |
          The will topic, present if the will flag is set.

      - id: will_message
        type: str
        encoding: utf-8
        size: will_message_length
        if: variable_header.connect_flags & 0x04 != 0
        doc: |
          The will message, present if the will flag is set.

      - id: username
        type: str
        encoding: utf-8
        size: username_length
        if: variable_header.connect_flags & 0x80 != 0
        doc: |
          The username, present if the username flag is set.

      - id: password
        type: str
        encoding: utf-8
        size: password_length
        if: variable_header.connect_flags & 0x40 != 0
        doc: |
          The password, present if the password flag is set.

instances:
  client_id_length:
    value: '2 + payload.client_id.length'

  will_topic_length:
    value: '2 + payload.will_topic.length'

  will_message_length:
    value: '2 + payload.will_message.length'

  username_length:
    value: '2 + payload.username.length'

  password_length:
    value: '2 + payload.password.length'

  vlq_base128:
    seq:
      - id: value
        type: u1
        repeat: until
        repeat-until: (value & 0x80) == 0
        doc: |
          Variable-length quantity encoding using base 128.