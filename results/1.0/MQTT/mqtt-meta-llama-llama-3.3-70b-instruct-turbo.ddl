domain mqtt {
  // Define the MQTT protocol domain
  protocol {
    // Define the MQTT protocol
    name = "MQTT"
    description = "Message Queuing Telemetry Transport"
    version = "3.3"
  }

  types {
    // Define the types used in the MQTT protocol
    uint8  : byte
    uint16 : uint16
    uint32 : uint32
    string : string
    bytes  : bytes
  }

  messages {
    // Define the MQTT message types
    connect {
      // Connect message
      type = uint8
      value = 1
      description = "Connect to the server"
      fields {
        protocol_name : string
        protocol_level : uint8
        connect_flags : uint8
        keep_alive : uint16
        client_id : string
        will_topic : string
        will_message : bytes
        username : string
        password : bytes
      }
    }

    connack {
      // Connack message
      type = uint8
      value = 2
      description = "Connection acknowledgement"
      fields {
        session_present : uint8
        return_code : uint8
      }
    }

    publish {
      // Publish message
      type = uint8
      value = 3
      description = "Publish a message"
      fields {
        topic_name : string
        packet_id : uint16
        payload : bytes
      }
    }

    puback {
      // Puback message
      type = uint8
      value = 4
      description = "Publish acknowledgement"
      fields {
        packet_id : uint16
      }
    }

    pubrec {
      // Pubrec message
      type = uint8
      value = 5
      description = "Publish received"
      fields {
        packet_id : uint16
      }
    }

    pubrel {
      // Pubrel message
      type = uint8
      value = 6
      description = "Publish release"
      fields {
        packet_id : uint16
      }
    }

    pubcomp {
      // Pubcomp message
      type = uint8
      value = 7
      description = "Publish complete"
      fields {
        packet_id : uint16
      }
    }

    subscribe {
      // Subscribe message
      type = uint8
      value = 8
      description = "Subscribe to a topic"
      fields {
        packet_id : uint16
        topics : array[string]
      }
    }

    suback {
      // Suback message
      type = uint8
      value = 9
      description = "Subscribe acknowledgement"
      fields {
        packet_id : uint16
        return_codes : array[uint8]
      }
    }

    unsubscribe {
      // Unsubscribe message
      type = uint8
      value = 10
      description = "Unsubscribe from a topic"
      fields {
        packet_id : uint16
        topics : array[string]
      }
    }

    unsuback {
      // Unsuback message
      type = uint8
      value = 11
      description = "Unsubscribe acknowledgement"
      fields {
        packet_id : uint16
      }
    }

    pingreq {
      // Pingreq message
      type = uint8
      value = 12
      description = "Ping request"
    }

    pingresp {
      // Pingresp message
      type = uint8
      value = 13
      description = "Ping response"
    }

    disconnect {
      // Disconnect message
      type = uint8
      value = 14
      description = "Disconnect from the server"
      fields {
        reason_code : uint8
        reason_string : string
      }
    }
  }

  encoder {
    // Define the encoder for the MQTT protocol
    type = "binary"
    options {
      // Options for the binary encoder
      byte_order = "big"
    }
  }

  decoder {
    // Define the decoder for the MQTT protocol
    type = "binary"
    options {
      // Options for the binary decoder
      byte_order = "big"
    }
  }
}