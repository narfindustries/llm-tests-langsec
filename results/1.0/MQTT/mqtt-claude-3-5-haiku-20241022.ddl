type uint4 = int[0, 15]
type uint2 = int[0, 3]

type mqtt_fixed_header = struct {
    packet_type: enum {
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
    },
    dup_flag: bool,
    qos_level: uint2,
    retain_flag: bool
}

type mqtt_property_value = variant<
    uint8,
    uint16,
    uint32,
    string,
    bytes
>

type mqtt_property = struct {
    property_id: enum {
        PAYLOAD_FORMAT_INDICATOR = 1,
        MESSAGE_EXPIRY_INTERVAL = 2,
        CONTENT_TYPE = 3,
        RESPONSE_TOPIC = 8,
        CORRELATION_DATA = 9,
        SUBSCRIPTION_IDENTIFIER = 11,
        SESSION_EXPIRY_INTERVAL = 17,
        ASSIGNED_CLIENT_ID = 18,
        SERVER_KEEP_ALIVE = 19,
        AUTHENTICATION_METHOD = 21,
        AUTHENTICATION_DATA = 22
    },
    value: mqtt_property_value
}

type mqtt_connect_packet = struct {
    fixed_header: mqtt_fixed_header,
    protocol_name: string,
    protocol_version: uint8,
    connect_flags: struct {
        username_flag: bool,
        password_flag: bool,
        will_retain: bool,
        will_qos: uint2,
        will_flag: bool,
        clean_start: bool
    },
    keep_alive: uint16,
    properties: list<mqtt_property>,
    client_id: string,
    username: optional<string>,
    password: optional<string>,
    will_properties: optional<list<mqtt_property>>,
    will_topic: optional<string>,
    will_payload: optional<bytes>
}

type mqtt_publish_packet = struct {
    fixed_header: mqtt_fixed_header,
    topic_name: string,
    packet_identifier: optional<uint16>,
    properties: list<mqtt_property>,
    payload: bytes
}

type mqtt_subscribe_packet = struct {
    fixed_header: mqtt_fixed_header,
    packet_identifier: uint16,
    properties: list<mqtt_property>,
    subscriptions: list<struct {
        topic_filter: string,
        subscription_options: struct {
            max_qos: uint2,
            no_local: bool,
            retain_as_published: bool,
            retain_handling: uint2
        }
    }>
}

type mqtt_suback_packet = struct {
    fixed_header: mqtt_fixed_header,
    packet_identifier: uint16,
    properties: list<mqtt_property>,
    reason_codes: list<uint8>
}

type mqtt_disconnect_packet = struct {
    fixed_header: mqtt_fixed_header,
    reason_code: uint8,
    properties: list<mqtt_property>
}

type mqtt_packet = variant<
    mqtt_connect_packet,
    mqtt_publish_packet,
    mqtt_subscribe_packet,
    mqtt_suback_packet,
    mqtt_disconnect_packet
>