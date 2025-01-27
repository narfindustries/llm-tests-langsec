struct MQTTFixedHeader {
    control_packet_type: uint8 {
        bits: 4;
    };
    flags: uint8 {
        bits: 4;
    };
    remaining_length: uint8 {
        while (this != 0) {
            this: uint8;
        }
    };
}

struct MQTTConnectPayload {
    client_id: string {
        length: 2;
        encoding: "utf8";
    };
    will_topic: string {
        length: 2;
        encoding: "utf8";
    };
    will_message: string {
        length: 2;
        encoding: "utf8";
    };
    username: string {
        length: 2;
        encoding: "utf8";
    };
    password: string {
        length: 2;
        encoding: "utf8";
    };
}

struct MQTTConnectVariableHeader {
    protocol_name: string {
        length: 2;
        encoding: "utf8";
    };
    protocol_level: uint8;
    connect_flags: uint8 {
        bits: 8;
    };
    keep_alive: uint16;
}

struct MQTTConnectPacket {
    fixed_header: MQTTFixedHeader;
    variable_header: MQTTConnectVariableHeader;
    payload: MQTTConnectPayload;
}

struct MQTTConnAckPacket {
    fixed_header: MQTTFixedHeader;
    session_present: uint8 {
        bits: 1;
    };
    return_code: uint8 {
        bits: 7;
    };
}

struct MQTTPublishPacket {
    fixed_header: MQTTFixedHeader;
    topic_name: string {
        length: 2;
        encoding: "utf8";
    };
    packet_id: uint16;
    payload: string {
        length: 2;
        encoding: "utf8";
    };
}

struct MQTTSubscribePacket {
    fixed_header: MQTTFixedHeader;
    packet_id: uint16;
    topic_filters: array {
        count: 2;
        element: string {
            length: 2;
            encoding: "utf8";
        };
    };
    requested_qos: array {
        count: 2;
        element: uint8 {
            bits: 2;
        };
    };
}

struct MQTTUnsubscribePacket {
    fixed_header: MQTTFixedHeader;
    packet_id: uint16;
    topic_filters: array {
        count: 2;
        element: string {
            length: 2;
            encoding: "utf8";
        };
    };
}

struct MQTTSubAckPacket {
    fixed_header: MQTTFixedHeader;
    packet_id: uint16;
    return_codes: array {
        count: 2;
        element: uint8 {
            bits: 8;
        };
    };
}

struct MQTTUnsubAckPacket {
    fixed_header: MQTTFixedHeader;
    packet_id: uint16;
}

struct MQTTPingReqPacket {
    fixed_header: MQTTFixedHeader;
}

struct MQTTPingRespPacket {
    fixed_header: MQTTFixedHeader;
}

struct MQTTDisconnectPacket {
    fixed_header: MQTTFixedHeader;
}

struct MQTTMessage {
    switch (fixed_header.control_packet_type) {
        case 1: connect: MQTTConnectPacket;
        case 2: connack: MQTTConnAckPacket;
        case 3: publish: MQTTPublishPacket;
        case 4: puback: MQTTFixedHeader;
        case 5: pubrec: MQTTFixedHeader;
        case 6: pubrel: MQTTFixedHeader;
        case 7: pubcomp: MQTTFixedHeader;
        case 8: subscribe: MQTTSubscribePacket;
        case 9: suback: MQTTSubAckPacket;
        case 10: unsubscribe: MQTTUnsubscribePacket;
        case 11: unsuback: MQTTUnsubAckPacket;
        case 12: pingreq: MQTTPingReqPacket;
        case 13: pingresp: MQTTPingRespPacket;
        case 14: disconnect: MQTTDisconnectPacket;
    }
}