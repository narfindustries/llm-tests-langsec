module MQTT {
    use Std = std;

    type MQTTPacketType = enum u8 {
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
        DISCONNECT = 14,
        AUTH = 15
    };

    type PropertyType = enum u8 {
        PAYLOAD_FORMAT_INDICATOR = 1,
        MESSAGE_EXPIRY_INTERVAL = 2,
        CONTENT_TYPE = 3,
        RESPONSE_TOPIC = 8,
        CORRELATION_DATA = 9,
        SUBSCRIPTION_IDENTIFIER = 11,
        SESSION_EXPIRY_INTERVAL = 17,
        ASSIGNED_CLIENT_IDENTIFIER = 18,
        SERVER_KEEP_ALIVE = 19,
        AUTHENTICATION_METHOD = 21,
        AUTHENTICATION_DATA = 22,
        REQUEST_PROBLEM_INFORMATION = 23,
        WILL_DELAY_INTERVAL = 24,
        REQUEST_RESPONSE_INFORMATION = 25,
        RESPONSE_INFORMATION = 26,
        SERVER_REFERENCE = 28,
        REASON_STRING = 31,
        RECEIVE_MAXIMUM = 33,
        TOPIC_ALIAS_MAXIMUM = 34,
        TOPIC_ALIAS = 35,
        MAXIMUM_QOS = 36,
        RETAIN_AVAILABLE = 37,
        USER_PROPERTY = 38,
        MAXIMUM_PACKET_SIZE = 39,
        WILDCARD_SUBSCRIPTION_AVAILABLE = 40,
        SUBSCRIPTION_IDENTIFIER_AVAILABLE = 41,
        SHARED_SUBSCRIPTION_AVAILABLE = 42
    };

    type ReasonCode = enum u8 {
        SUCCESS = 0x00,
        NORMAL_DISCONNECTION = 0x00,
        GRANTED_QOS_0 = 0x00,
        GRANTED_QOS_1 = 0x01,
        GRANTED_QOS_2 = 0x02,
        DISCONNECT_WITH_WILL_MESSAGE = 0x04,
        NO_MATCHING_SUBSCRIBERS = 0x10,
        NO_SUBSCRIPTION_EXISTED = 0x11,
        CONTINUE_AUTHENTICATION = 0x18,
        REAUTHENTICATE = 0x19,
        UNSPECIFIED_ERROR = 0x80,
        MALFORMED_PACKET = 0x81,
        PROTOCOL_ERROR = 0x82,
        IMPLEMENTATION_SPECIFIC_ERROR = 0x83,
        UNSUPPORTED_PROTOCOL_VERSION = 0x84,
        CLIENT_IDENTIFIER_NOT_VALID = 0x85,
        BAD_USERNAME_OR_PASSWORD = 0x86,
        NOT_AUTHORIZED = 0x87,
        SERVER_UNAVAILABLE = 0x88,
        SERVER_BUSY = 0x89,
        BANNED = 0x8A,
        SERVER_SHUTTING_DOWN = 0x8B,
        BAD_AUTHENTICATION_METHOD = 0x8C,
        KEEP_ALIVE_TIMEOUT = 0x8D,
        SESSION_TAKEN_OVER = 0x8E,
        TOPIC_FILTER_INVALID = 0x8F,
        TOPIC_NAME_INVALID = 0x90,
        PACKET_IDENTIFIER_IN_USE = 0x91,
        PACKET_IDENTIFIER_NOT_FOUND = 0x92,
        RECEIVE_MAXIMUM_EXCEEDED = 0x93,
        TOPIC_ALIAS_INVALID = 0x94,
        PACKET_TOO_LARGE = 0x95,
        MESSAGE_RATE_TOO_HIGH = 0x96,
        QUOTA_EXCEEDED = 0x97,
        ADMINISTRATIVE_ACTION = 0x98,
        PAYLOAD_FORMAT_INVALID = 0x99,
        RETAIN_NOT_SUPPORTED = 0x9A,
        QOS_NOT_SUPPORTED = 0x9B,
        SHARED_SUBSCRIPTIONS_NOT_SUPPORTED = 0x9C,
        CONNECTION_RATE_EXCEEDED = 0x9D
    };

    type Property = struct {
        identifier: PropertyType,
        length: u32,
        value: bytes(length)
    };

    type ConnectFlags = bitfield u8 {
        reserved: 1,
        cleanStart: 1,
        willFlag: 1,
        willQoS: 2,
        willRetain: 1,
        passwordFlag: 1,
        usernameFlag: 1
    };

    type Connect = struct {
        protocolName: string,
        protocolLevel: u8,
        connectFlags: ConnectFlags,
        keepAlive: u16,
        propertyLength: u32,
        properties: list<Property>(propertyLength),
        clientId: string,
        willTopic: option<string>,
        willPayload: option<bytes>,
        username: option<string>,
        password: option<string>
    };

    type Connack = struct {
        sessionPresent: bool,
        reasonCode: ReasonCode,
        propertyLength: u32,
        properties: list<Property>(propertyLength)
    };

    type Publish = struct {
        topicName: string,
        packetIdentifier: option<u16>,
        propertyLength: u32,
        properties: list<Property>(propertyLength),
        payload: bytes,
        qosLevel: u2,
        retain: bool,
        duplicate: bool
    };

    type Subscribe = struct {
        packetIdentifier: u16,
        propertyLength: u32,
        properties: list<Property>(propertyLength),
        topicFilters: list<(string, u8)>
    };

    type Suback = struct {
        packetIdentifier: u16,
        propertyLength: u32,
        properties: list<Property>(propertyLength),
        reasonCodes: list<ReasonCode>
    };

    type Unsubscribe = struct {
        packetIdentifier: u16,
        propertyLength: u32,
        properties: list<Property>(propertyLength),
        topicFilters: list<string>
    };

    type Unsuback = struct {
        packetIdentifier: u16,
        propertyLength: u32,
        properties: list<Property>(propertyLength),
        reasonCodes: list<ReasonCode>
    };

    type Disconnect = struct {
        reasonCode: ReasonCode,
        propertyLength: u32,
        properties: list<Property>(propertyLength)
    };

    type Auth = struct {
        reasonCode: ReasonCode,
        propertyLength: u32,
        properties: list<Property>(propertyLength)
    };

    type MQTTPacket = struct {
        packetType: MQTTPacketType,
        packet: variant {
            Connect: Connect,
            Connack: Connack,
            Publish: Publish,
            Puback: (u16, ReasonCode, u32, list<Property>),
            Pubrec: (u16, ReasonCode, u32, list<Property>),
            Pubrel: (u16, ReasonCode, u32, list<Property>),
            Pubcomp: (u16, ReasonCode, u32, list<Property>),
            Subscribe: Subscribe,
            Suback: Suback,
            Unsubscribe: Unsubscribe,
            Unsuback: Unsuback,
            Pingreq: unit,
            Pingresp: unit,
            Disconnect: Disconnect,
            Auth: Auth
        }
    };
}