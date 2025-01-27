domain MQTT {
  type byte = uint8
  type uint16 = uint16
  type uint32 = uint32
  type varint = uint32

  type connect_flags {
    username: bool
    password: bool
    will_retain: bool
    will_qos: uint2
    will_flag: bool
    clean_session: bool
    reserved: uint1
  }

  type connect_packet {
    header: byte
    remaining_length: varint
    protocol_name: string(6)
    protocol_level: byte
    connect_flags: connect_flags
    keep_alive: uint16
    client_id: string
    will_topic: string
    will_message: string
    username: string
    password: string
  }

  type connack_packet {
    header: byte
    remaining_length: varint
    connect_ack_flags: byte
    connect_reason_code: byte
  }

  type publish_packet {
    header: byte
    remaining_length: varint
    topic_name: string
    packet_id: uint16
    payload: bytes
  }

  type puback_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
  }

  type pubrec_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
  }

  type pubrel_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
  }

  type pubcomp_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
  }

  type subscribe_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
    topic_filters: array(string)
  }

  type suback_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
    return_codes: array(byte)
  }

  type unsubscribe_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
    topic_filters: array(string)
  }

  type unsuback_packet {
    header: byte
    remaining_length: varint
    packet_id: uint16
  }

  type pingreq_packet {
    header: byte
    remaining_length: varint
  }

  type pingresp_packet {
    header: byte
    remaining_length: varint
  }

  type disconnect_packet {
    header: byte
    remaining_length: varint
    disconnect_reason_code: byte
  }
}