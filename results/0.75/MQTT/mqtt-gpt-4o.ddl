enum uint8 MQTTControlPacketType {
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

struct MQTTFixedHeader {
    MQTTControlPacketType packetType : 4;
    uint8 flags : 4;
    varuint remainingLength;
}

struct MQTTString {
    uint16 length;
    char[length] value;
}

struct MQTTBinaryData {
    uint16 length;
    uint8[length] value;
}

struct MQTTVariableByteInteger {
    varuint value;
}

struct MQTTProperties {
    MQTTVariableByteInteger propertyLength;
    Property[] properties;
}

struct Property {
    uint8 identifier;
    switch (identifier) {
        case 0x11: uint32 sessionExpiryInterval;
        case 0x21: uint16 receiveMaximum;
        case 0x27: uint32 maximumPacketSize;
        case 0x22: uint16 topicAliasMaximum;
        case 0x19: bool requestResponseInformation;
        case 0x17: bool requestProblemInformation;
        case 0x26: UserProperty userProperty;
        case 0x15: MQTTString authenticationMethod;
        case 0x16: MQTTBinaryData authenticationData;
        case 0x18: uint32 willDelayInterval;
        case 0x01: bool payloadFormatIndicator;
        case 0x02: uint32 messageExpiryInterval;
        case 0x03: MQTTString contentType;
        case 0x08: MQTTString responseTopic;
        case 0x09: MQTTBinaryData correlationData;
        case 0x1F: uint8 maximumQoS;
        case 0x25: bool retainAvailable;
        case 0x12: MQTTString assignedClientIdentifier;
        case 0x24: bool wildcardSubscriptionAvailable;
        case 0x28: bool subscriptionIdentifiersAvailable;
        case 0x29: bool sharedSubscriptionsAvailable;
        case 0x13: uint16 serverKeepAlive;
        case 0x1A: MQTTString responseInformation;
        case 0x1C: MQTTString serverReference;
        case 0x1E: MQTTString reasonString;
    }
}

struct UserProperty {
    MQTTString key;
    MQTTString value;
}

struct MQTTConnectPacket {
    MQTTFixedHeader header;
    MQTTString protocolName;
    uint8 protocolLevel;
    uint8 connectFlags;
    uint16 keepAlive;
    MQTTProperties connectProperties;
    MQTTString clientIdentifier;
    if connectFlags & 0x04 {
        MQTTProperties willProperties;
        MQTTString willTopic;
        MQTTBinaryData willPayload;
    }
    if connectFlags & 0x80 {
        MQTTString username;
    }
    if connectFlags & 0x40 {
        MQTTBinaryData password;
    }
}

struct MQTTConnackPacket {
    MQTTFixedHeader header;
    uint8 connackFlags;
    uint8 reasonCode;
    MQTTProperties connackProperties;
}

struct MQTTPublishPacket {
    MQTTFixedHeader header;
    MQTTString topicName;
    if (header.flags & 0x06) {
        uint16 packetIdentifier;
    }
    MQTTProperties publishProperties;
    uint8[] payload;
}

struct MQTTPubackPacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    uint8 reasonCode;
    MQTTProperties pubackProperties;
}

struct MQTTPubrecPacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    uint8 reasonCode;
    MQTTProperties pubrecProperties;
}

struct MQTTPubrelPacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    uint8 reasonCode;
    MQTTProperties pubrelProperties;
}

struct MQTTPubcompPacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    uint8 reasonCode;
    MQTTProperties pubcompProperties;
}

struct MQTTSubscribePacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    MQTTProperties subscribeProperties;
    Subscription[] subscriptions;
}

struct Subscription {
    MQTTString topicFilter;
    uint8 subscriptionOptions;
}

struct MQTTSubackPacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    MQTTProperties subackProperties;
    uint8[] reasonCodes;
}

struct MQTTUnsubscribePacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    MQTTProperties unsubscribeProperties;
    MQTTString[] topicFilters;
}

struct MQTTUnsubackPacket {
    MQTTFixedHeader header;
    uint16 packetIdentifier;
    MQTTProperties unsubackProperties;
    uint8[] reasonCodes;
}

struct MQTTPingreqPacket {
    MQTTFixedHeader header;
}

struct MQTTPingrespPacket {
    MQTTFixedHeader header;
}

struct MQTTDisconnectPacket {
    MQTTFixedHeader header;
    uint8 reasonCode;
    MQTTProperties disconnectProperties;
}

struct MQTTAuthPacket {
    MQTTFixedHeader header;
    uint8 reasonCode;
    MQTTProperties authProperties;
}

struct MQTTPacket {
    MQTTFixedHeader header;
    switch (header.packetType) {
        case MQTTControlPacketType.CONNECT: MQTTConnectPacket connect;
        case MQTTControlPacketType.CONNACK: MQTTConnackPacket connack;
        case MQTTControlPacketType.PUBLISH: MQTTPublishPacket publish;
        case MQTTControlPacketType.PUBACK: MQTTPubackPacket puback;
        case MQTTControlPacketType.PUBREC: MQTTPubrecPacket pubrec;
        case MQTTControlPacketType.PUBREL: MQTTPubrelPacket pubrel;
        case MQTTControlPacketType.PUBCOMP: MQTTPubcompPacket pubcomp;
        case MQTTControlPacketType.SUBSCRIBE: MQTTSubscribePacket subscribe;
        case MQTTControlPacketType.SUBACK: MQTTSubackPacket suback;
        case MQTTControlPacketType.UNSUBSCRIBE: MQTTUnsubscribePacket unsubscribe;
        case MQTTControlPacketType.UNSUBACK: MQTTUnsubackPacket unsuback;
        case MQTTControlPacketType.PINGREQ: MQTTPingreqPacket pingreq;
        case MQTTControlPacketType.PINGRESP: MQTTPingrespPacket pingresp;
        case MQTTControlPacketType.DISCONNECT: MQTTDisconnectPacket disconnect;
        case MQTTControlPacketType.AUTH: MQTTAuthPacket auth;
    }
}