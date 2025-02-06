enum ControlPacketType : uint8 {
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
}

struct MQTT {
    ControlPacketType packetType;
    uint8 flags;
    uint32 remainingLength;
    switch (packetType) {
        case CONNECT: ConnectPacket connect;
        case CONNACK: ConnackPacket connack;
        case PUBLISH: PublishPacket publish;
        case PUBACK: PubackPacket puback;
        case PUBREC: PubrecPacket pubrec;
        case PUBREL: PubrelPacket pubrel;
        case PUBCOMP: PubcompPacket pubcomp;
        case SUBSCRIBE: SubscribePacket subscribe;
        case SUBACK: SubackPacket suback;
        case UNSUBSCRIBE: UnsubscribePacket unsubscribe;
        case UNSUBACK: UnsubackPacket unsuback;
        case PINGREQ: PingreqPacket pingreq;
        case PINGRESP: PingrespPacket pingresp;
        case DISCONNECT: DisconnectPacket disconnect;
        case AUTH: AuthPacket auth;
    }
}

struct ConnectPacket {
    string protocolName;
    uint8 protocolLevel;
    uint8 connectFlags;
    uint16 keepAlive;
    Properties properties;
    string clientIdentifier;
    if (connectFlags & 0x04) {
        uint8 willPropertiesLength;
        WillProperties willProperties;
        string willTopic;
        bytes willPayload;
    }
    if (connectFlags & 0x80) {
        string username;
    }
    if (connectFlags & 0x40) {
        bytes password;
    }
}

struct ConnackPacket {
    uint8 connackFlags;
    uint8 reasonCode;
    Properties properties;
}

struct PublishPacket {
    string topicName;
    if ((flags & 0x06) != 0) {
        uint16 packetIdentifier;
    }
    Properties properties;
    bytes payload;
}

struct PubackPacket {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Properties properties;
}

struct PubrecPacket {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Properties properties;
}

struct PubrelPacket {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Properties properties;
}

struct PubcompPacket {
    uint16 packetIdentifier;
    uint8 reasonCode;
    Properties properties;
}

struct SubscribePacket {
    uint16 packetIdentifier;
    Properties properties;
    list<Subscription> subscriptions;
}

struct Subscription {
    string topicFilter;
    uint8 subscriptionOptions;
}

struct SubackPacket {
    uint16 packetIdentifier;
    Properties properties;
    list<uint8> reasonCodes;
}

struct UnsubscribePacket {
    uint16 packetIdentifier;
    Properties properties;
    list<string> topicFilters;
}

struct UnsubackPacket {
    uint16 packetIdentifier;
    Properties properties;
    list<uint8> reasonCodes;
}

struct PingreqPacket {}

struct PingrespPacket {}

struct DisconnectPacket {
    uint8 reasonCode;
    Properties properties;
}

struct AuthPacket {
    uint8 reasonCode;
    Properties properties;
}

struct Properties {
    optional uint32 sessionExpiryInterval;
    optional uint16 receiveMaximum;
    optional uint32 maximumPacketSize;
    optional uint16 topicAliasMaximum;
    optional uint8 requestResponseInformation;
    optional uint8 requestProblemInformation;
    list<UserProperty> userProperties;
    optional string authenticationMethod;
    optional bytes authenticationData;
}

struct WillProperties {
    optional uint32 willDelayInterval;
    optional uint8 payloadFormatIndicator;
    optional uint32 messageExpiryInterval;
    optional string contentType;
    optional string responseTopic;
    optional bytes correlationData;
    list<UserProperty> userProperties;
}

struct UserProperty {
    string name;
    string value;
}