enum PacketType : uint8 {
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
    PacketType packetType;
    uint8 flags;
    uint32 remainingLength;
    switch (packetType) {
        case PacketType.CONNECT: ConnectPacket connect;
        case PacketType.CONNACK: ConnackPacket connack;
        case PacketType.PUBLISH: PublishPacket publish;
        case PacketType.PUBACK: PubackPacket puback;
        case PacketType.PUBREC: PubrecPacket pubrec;
        case PacketType.PUBREL: PubrelPacket pubrel;
        case PacketType.PUBCOMP: PubcompPacket pubcomp;
        case PacketType.SUBSCRIBE: SubscribePacket subscribe;
        case PacketType.SUBACK: SubackPacket suback;
        case PacketType.UNSUBSCRIBE: UnsubscribePacket unsubscribe;
        case PacketType.UNSUBACK: UnsubackPacket unsuback;
        case PacketType.PINGREQ: PingreqPacket pingreq;
        case PacketType.PINGRESP: PingrespPacket pingresp;
        case PacketType.DISCONNECT: DisconnectPacket disconnect;
        case PacketType.AUTH: AuthPacket auth;
    }
}

struct ConnectPacket {
    string protocolName;
    uint8 protocolLevel;
    uint8 connectFlags;
    uint16 keepAlive;
    Properties properties;
    string clientIdentifier;
    optional {
        uint8 willPropertiesLength;
        Properties willProperties;
        string willTopic;
        bytes willPayload;
    } if (connectFlags & 0x04);
    optional string username if (connectFlags & 0x80);
    optional bytes password if (connectFlags & 0x40);
}

struct ConnackPacket {
    uint8 sessionPresent;
    uint8 connectReasonCode;
    Properties properties;
}

struct PublishPacket {
    string topicName;
    optional uint16 packetIdentifier if ((flags & 0x06) != 0);
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
    repeated Subscription subscriptions;
}

struct Subscription {
    string topicFilter;
    uint8 subscriptionOptions;
}

struct SubackPacket {
    uint16 packetIdentifier;
    Properties properties;
    repeated uint8 reasonCodes;
}

struct UnsubscribePacket {
    uint16 packetIdentifier;
    Properties properties;
    repeated string topicFilters;
}

struct UnsubackPacket {
    uint16 packetIdentifier;
    Properties properties;
    repeated uint8 reasonCodes;
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
    repeated Property properties;
}

struct Property {
    uint8 identifier;
    switch (identifier) {
        case 0x01: uint8 payloadFormatIndicator;
        case 0x02: uint32 messageExpiryInterval;
        case 0x03: string contentType;
        case 0x08: string responseTopic;
        case 0x09: bytes correlationData;
        case 0x0B: uint32 subscriptionIdentifier;
        case 0x11: uint32 sessionExpiryInterval;
        case 0x12: uint16 receiveMaximum;
        case 0x13: uint16 topicAliasMaximum;
        case 0x15: uint32 maximumPacketSize;
        case 0x17: uint8 requestResponseInformation;
        case 0x18: uint8 requestProblemInformation;
        case 0x19: string userPropertyName;
        case 0x1A: string userPropertyValue;
        case 0x1F: uint16 topicAlias;
        case 0x21: uint8 maximumQoS;
        case 0x22: uint8 retainAvailable;
        case 0x23: uint8 wildcardSubscriptionAvailable;
        case 0x24: uint8 subscriptionIdentifierAvailable;
        case 0x25: uint8 sharedSubscriptionAvailable;
    }
}