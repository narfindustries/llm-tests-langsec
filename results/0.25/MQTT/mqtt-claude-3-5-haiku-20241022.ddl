grammar MQTT {
    protocol = packet*;

    packet = connect | connack | publish | puback | pubrec | pubrel | pubcomp |
             subscribe | suback | unsubscribe | unsuback | pingreq | pingresp | disconnect;

    type ProtocolName = "MQTT";
    type ProtocolVersion = 5;

    enum PacketType {
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

    enum ReasonCode {
        SUCCESS = 0,
        NORMAL_DISCONNECT = 0,
        GRANTED_QOS0 = 0,
        GRANTED_QOS1 = 1,
        GRANTED_QOS2 = 2,
        DISCONNECT_WITH_WILL_MESSAGE = 4,
        NO_MATCHING_SUBSCRIBERS = 16,
        NO_SUBSCRIPTION_EXISTED = 17,
        CONTINUE_AUTHENTICATION = 24,
        REAUTHENTICATE = 25,
        UNSPECIFIED_ERROR = 128,
        MALFORMED_PACKET = 129,
        PROTOCOL_ERROR = 130,
        IMPLEMENTATION_SPECIFIC_ERROR = 131,
        UNSUPPORTED_PROTOCOL_VERSION = 132,
        CLIENT_IDENTIFIER_NOT_VALID = 133,
        BAD_USERNAME_OR_PASSWORD = 134,
        NOT_AUTHORIZED = 135,
        SERVER_UNAVAILABLE = 136,
        SERVER_BUSY = 137,
        BANNED = 138,
        SERVER_SHUTTING_DOWN = 139,
        BAD_AUTHENTICATION_METHOD = 140,
        KEEP_ALIVE_TIMEOUT = 141,
        SESSION_TAKEN_OVER = 142,
        TOPIC_FILTER_INVALID = 143,
        TOPIC_NAME_INVALID = 144,
        PACKET_IDENTIFIER_IN_USE = 145,
        PACKET_IDENTIFIER_NOT_FOUND = 146,
        RECEIVE_MAXIMUM_EXCEEDED = 147,
        TOPIC_ALIAS_INVALID = 148,
        PACKET_TOO_LARGE = 149,
        MESSAGE_RATE_TOO_HIGH = 150,
        QUOTA_EXCEEDED = 151,
        ADMINISTRATIVE_ACTION = 152,
        PAYLOAD_FORMAT_INVALID = 153,
        RETAIN_NOT_SUPPORTED = 154,
        QOS_NOT_SUPPORTED = 155,
        SHARED_SUBSCRIPTIONS_NOT_SUPPORTED = 156,
        CONNECTION_RATE_EXCEEDED = 157,
        MAXIMUM_CONNECT_TIME = 158,
        SUBSCRIPTION_IDENTIFIERS_NOT_SUPPORTED = 159,
        WILDCARD_SUBSCRIPTIONS_NOT_SUPPORTED = 160
    }

    enum PropertyType {
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
    }

    type VarInt = uint32;

    type Property {
        type: PropertyType,
        value: variant {
            PayloadFormatIndicator: uint8,
            MessageExpiryInterval: uint32,
            ContentType: string,
            ResponseTopic: string,
            CorrelationData: bytes,
            SubscriptionIdentifier: VarInt,
            SessionExpiryInterval: uint32,
            AssignedClientIdentifier: string,
            ServerKeepAlive: uint16,
            AuthenticationMethod: string,
            AuthenticationData: bytes,
            RequestProblemInformation: uint8,
            WillDelayInterval: uint32,
            RequestResponseInformation: uint8,
            ResponseInformation: string,
            ServerReference: string,
            ReasonString: string,
            ReceiveMaximum: uint16,
            TopicAliasMaximum: uint16,
            TopicAlias: uint16,
            MaximumQoS: uint8,
            RetainAvailable: uint8,
            UserProperty: (string, string),
            MaximumPacketSize: uint32,
            WildcardSubscriptionAvailable: uint8,
            SubscriptionIdentifierAvailable: uint8,
            SharedSubscriptionAvailable: uint8
        }
    }

    type FixedHeader {
        packetType: PacketType,
        flags: uint4,
        remainingLength: VarInt
    }

    type ConnectFlags {
        username: bool,
        password: bool,
        willRetain: bool,
        willQoS: uint2,
        willFlag: bool,
        cleanStart: bool
    }

    type Connect {
        fixedHeader: FixedHeader,
        protocolName: ProtocolName,
        protocolVersion: ProtocolVersion,
        connectFlags: ConnectFlags,
        keepAlive: uint16,
        properties: list[Property],
        clientId: string,
        willProperties: optional[list[Property]],
        willTopic: optional[string],
        willPayload: optional[bytes],
        username: optional[string],
        password: optional[string]
    }

    type Connack {
        fixedHeader: FixedHeader,
        connectAcknowledgeFlags: uint8,
        reasonCode: ReasonCode,
        properties: list[Property]
    }

    type Publish {
        fixedHeader: FixedHeader,
        topicName: string,
        packetIdentifier: optional[uint16],
        properties: list[Property],
        payload: bytes
    }

    type Puback {
        fixedHeader: FixedHeader,
        packetIdentifier: uint16,
        reasonCode: ReasonCode,
        properties: list[Property]
    }

    type Subscribe {
        fixedHeader: FixedHeader,
        packetIdentifier: uint16,
        properties: list[Property],
        topicFilters: list[(string, uint8)]
    }

    type Suback {
        fixedHeader: FixedHeader,
        packetIdentifier: uint16,
        properties: list[Property],
        reasonCodes: list[ReasonCode]
    }

    type Unsubscribe {
        fixedHeader: FixedHeader,
        packetIdentifier: uint16,
        properties: list[Property],
        topicFilters: list[string]
    }

    type Unsuback {
        fixedHeader: FixedHeader,
        packetIdentifier: uint16,
        properties: list[Property],
        reasonCodes: list[ReasonCode]
    }

    type Disconnect {
        fixedHeader: FixedHeader,
        reasonCode: ReasonCode,
        properties: list[Property]
    }

    type Pingreq {
        fixedHeader: FixedHeader
    }

    type Pingresp {
        fixedHeader: FixedHeader
    }

    type Packet = variant {
        Connect: Connect,
        Connack: Connack,
        Publish: Publish,
        Puback: Puback,
        Pubrec: Puback,
        Pubrel: Puback,
        Pubcomp: Puback,
        Subscribe: Subscribe,
        Suback: Suback,
        Unsubscribe: Unsubscribe,
        Unsuback: Unsuback,
        Pingreq: Pingreq,
        Pingresp: Pingresp,
        Disconnect: Disconnect
    }
}