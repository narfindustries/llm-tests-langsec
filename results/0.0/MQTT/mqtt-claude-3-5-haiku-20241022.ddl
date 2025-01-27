// MQTT Protocol Specification
protocol MQTT {
    // MQTT Packet Types
    type PacketType enum {
        CONNECT = 1,
        CONNACK = 2,
        PUBLISH = 3,
        PUBACK = 4,
        PUBREC = 5,
        PUBREL = 6,
        PUBCOMP = 7,
        SUBSCRIBE = 8,
        SUBACK = 9,
        UNSUBSCRIBE = 10,
        UNSUBACK = 11,
        PINGREQ = 12,
        PINGRESP = 13,
        DISCONNECT = 14
    }

    // Fixed Header Structure
    type FixedHeader {
        packet_type: PacketType,
        dup_flag: bool,
        qos_level: uint(2),
        retain_flag: bool,
        remaining_length: varint
    }

    // Connect Packet Specification
    type ConnectPacket {
        fixed_header: FixedHeader,
        protocol_name: string(length=4),
        protocol_level: uint(8),
        connect_flags: {
            username_flag: bool,
            password_flag: bool,
            will_retain: bool,
            will_qos: uint(2),
            will_flag: bool,
            clean_session: bool,
            reserved: bool
        },
        keep_alive: uint(16),
        client_id: string,
        will_topic?: string,
        will_message?: bytes,
        username?: string,
        password?: string
    }

    // Publish Packet Specification
    type PublishPacket {
        fixed_header: FixedHeader,
        topic_name: string,
        packet_id?: uint(16),
        payload: bytes
    }

    // Subscribe Packet Specification
    type SubscribePacket {
        fixed_header: FixedHeader,
        packet_id: uint(16),
        topics: list<{
            topic_filter: string,
            max_qos: uint(2)
        }>
    }

    // Validation Rules
    constraints {
        // Protocol Name must be 'MQTT'
        ConnectPacket.protocol_name == "MQTT",
        
        // Protocol Level must be 4 (MQTT 3.1.1)
        ConnectPacket.protocol_level == 4,
        
        // Topic names cannot contain wildcards in PUBLISH
        !PublishPacket.topic_name.contains("+") && 
        !PublishPacket.topic_name.contains("#"),
        
        // QoS levels are 0, 1, or 2
        PublishPacket.fixed_header.qos_level <= 2,
        SubscribePacket.topics.max_qos <= 2
    }

    // Parsing and Generation Rules
    format {
        // Fixed Header Encoding
        FixedHeader: {
            first_byte: uint(8) = 
                (packet_type << 4) | 
                (dup_flag ? 0x08 : 0) | 
                (qos_level << 1) | 
                (retain_flag ? 0x01 : 0),
            remaining_length: varint
        }
    }
}